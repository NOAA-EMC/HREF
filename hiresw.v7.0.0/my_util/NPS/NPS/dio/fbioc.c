#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#define _LARGE_FILES

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <string.h>
#include <stdint.h>

#include "fbioc.h"

const uint32_t fortranheader = 4;

void c_endian(int *endian)
{
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01020304};

    if (bint.c[0] == 1) 
      *endian = 0;   /*    big endian */
    else
      *endian = 1;   /* little endian */
}

void c_swap(char *a, const int width, const int size)
{
    int  i, j;
    char swap[8];

    if (width == 1) return;

    if (width != 2 && width != 4 && width != 8) {
      fprintf(stderr,"swap: width in not 1,2,4 or 8 width=%d size=%d\n",width,size);
      exit(1);
    }

    for (j=0; j<size; j++) {
      for (i=0; i<width; i++) swap[i] = a[j*width+i];
      for (i=0; i<width; i++) a[j*width+i] = swap[width-i-1];
    }
}

void c_fbopen (int *iunit, char *fname, int *flen, char *mode, int *ierr)
{

   char *lfname = NULL;
   int len;
   int oflag = -1;

   lfname = (char *) malloc(*flen+1);
   if (!lfname) {
      perror("malloc");
      *ierr = -1;
   }
   strncpy(lfname,fname,*flen);
   len = *flen;
   while (len && lfname[--len] == ' ');
   lfname[++len]='\0';

   if ( *mode == 'R') oflag = O_RDONLY;
   if ( *mode == 'W') oflag = O_RDWR | O_CREAT | O_TRUNC;
   if ( *mode == 'O') oflag = O_RDWR;

   if ( oflag == -1 ) {
      fprintf(stderr," Wrong mode in fbopen %s \n", mode);
      *ierr = -1;
   }

   *iunit = open(lfname,oflag,0666);

   if (*iunit == -1) {
      fprintf(stderr," Error Open File: %s \n",lfname);
      *ierr = -1;
   } else {
      fprintf(stderr," File: %s opened \n",lfname);
      *ierr = 0;
   }

   if (lfname) free(lfname);
}

void c_fbread (int *iunit, char *buffer, const int *width, const int *size, const int *swap, int *ierr)
{

   int rd, rd4;
   uint32_t reclen;
   uint32_t reclen1, reclen2;

   *ierr = 0;

   reclen = (*width) * (*size);

   rd4 = read(*iunit,(void *)&reclen1,fortranheader);
   if ( *swap == 1 ) c_swap((char *)&reclen1,fortranheader,1);
   if (reclen1 != reclen) {
      fprintf(stderr," reclen1 = %d  reclen = %d  rd = %d \n", reclen1, reclen, rd4);
      fprintf(stderr," error in c_fbread 1 \n");
      *ierr = -1;
   }

   rd = read(*iunit,buffer,reclen);
   if (rd != reclen) {
      fprintf(stderr," rd = %d   reclen = %d \n", rd, reclen);
      fprintf(stderr," error in c_fbread 2 \n");
      *ierr = -1;
   }
   if ( *swap == 1 ) c_swap(buffer,*width,*size);

   rd4 = read(*iunit,(void *)&reclen2,fortranheader);
   if ( *swap == 1 ) c_swap((char *)&reclen2,fortranheader,1);
   if (reclen2 != reclen1) {
      fprintf(stderr," reclen1 = %d  reclen2 = %d \n", reclen1, reclen2);
      fprintf(stderr," error in c_fbread 3 \n");
      *ierr = -1;
   }

}

void c_fbread_4bytes (int *iunit, char *buffer, int *ierr)
{
   int rd4;
   rd4 = read(*iunit,buffer,4);
   if (rd4 != 4) {
      fprintf(stderr,"c_fbread_4bytes: rd4 = %d not equal 4 \n", rd4);
      fprintf(stderr," error in c_fbread_4bytes \n");
      *ierr = -1;
   }
}

void c_fbwrite (int *iunit, char *buffer, const int *width, const int *size, const int *swap, int *ierr)
{

   int rd, rd4;
   off_t before, after;
   uint32_t reclen,real_reclen;
   char *swapped_buffer;

   *ierr = 0;
  
   real_reclen = (*width) * (*size);
   reclen = real_reclen;
   if ( *swap == 1 ) c_swap((char *)&reclen,fortranheader,1);       
   rd4 = write(*iunit,(char *)&reclen,fortranheader);

   if ( *swap == 1 ) {
      swapped_buffer = (char *)malloc(real_reclen);
      memcpy(swapped_buffer,buffer,real_reclen);
      c_swap(swapped_buffer,*width,*size);
      rd = write(*iunit,swapped_buffer,real_reclen);
      free(swapped_buffer);
   } else {
      rd = write(*iunit,buffer,real_reclen);
   }

   rd4 = write(*iunit,(char *)&reclen,fortranheader);

   if (rd != real_reclen) {
      fprintf(stderr," rd = %d   real_reclen = %d \n", rd, real_reclen);
      fprintf(stderr," Error in fbwrite \n");
      *ierr = -1;
   }

}


void c_fbclose (int *iunit, int *ierr)
{
   *ierr = close(*iunit);
}

void c_fbseekset (int *iunit, uint64_t *seek, int *ierr)
{
   off_t seek_result;
   seek_result = lseek(*iunit, *seek, SEEK_SET);
   if ( seek_result != *seek ) {
      fprintf(stderr," in c_fbseekset seek_result = %ld   seek = %"PRIu64" \n", seek_result, *seek );
      *ierr = -1;
   } else {
      *ierr = 0;
   }
}

void c_fbseekcur (int *iunit, uint64_t *seek, int *ierr)
{
   *ierr = lseek(*iunit, *seek, SEEK_CUR);
}

void c_fbseek_record (int *iunit, int *swap, int *ierr)
{
   int rd4;
   uint32_t reclen1, reclen2;
   off_t seek_result;

   *ierr = 0;

   rd4 = read(*iunit,(char *)&reclen1,fortranheader);
   if ( *swap == 1 ) c_swap((char *)&reclen1,fortranheader,1);
   if (rd4 != fortranheader) {
       *ierr=1;
       return;
   }

   seek_result = lseek(*iunit, (off_t) reclen1, SEEK_CUR);

   rd4 = read(*iunit,(char *)&reclen2,fortranheader);
   if ( *swap == 1 ) c_swap((char *)&reclen2,fortranheader,1);
   if (rd4 != fortranheader) {
       *ierr=1;
       return;
   }
   if ( reclen1 != reclen2 ) {
      fprintf(stderr," in c_fbseek_record reclen1 = %d reclen2 = %d \n", reclen1,  reclen2);
      *ierr = -1;
   }
}

void c_fbseekend (int *iunit, uint64_t *seek, int *ierr)
{
   *ierr = lseek(*iunit, *seek, SEEK_END);
}

void c_fbtell (int *iunit, uint64_t *offset, int *ierr)
{
   *offset = lseek(*iunit, (off_t)0, SEEK_CUR);
   *ierr = 0;
}

void c_fbsize (int *iunit, uint64_t *size, int *ierr)
{
   *size = lseek(*iunit, (off_t)0, SEEK_END);
   *ierr = 0;
}


void c_fbwrite_record (int *iunit, char *name, int *rank, int *dtype, int *dsize, int *bounds, char *data, int *header, int *swap, int *ierr)
{
    int r;
    int width, size;

    /* name */
    width=1;
    size=32;
    c_fbwrite (iunit,name,&width,&size,swap,ierr);

    /* rank */
    width=sizeof(*rank);
    size=1;
    c_fbwrite (iunit,(char *)rank,&width,&size,swap,ierr);

    /* dtype */
    width=sizeof(*dtype);
    size=1;
    c_fbwrite (iunit,(char *)dtype,&width,&size,swap,ierr);

    /* bounds */
    width=4;
    size=2*(*rank);
    c_fbwrite (iunit,(char *)bounds,&width,&size,swap,ierr);

    /* header */
    width=4;
    size=512;
    c_fbwrite (iunit,(char *)header,&width,&size,swap,ierr);

    /* data */
    size = 1;
    for (r=0; r<*rank; r++) {
        size = size * (bounds[r*2+1] - bounds[r*2+0] + 1);
    }
    c_fbwrite (iunit,(char *)data,dsize,&size,swap,ierr);
}

void c_fbread_record (int *iunit, char *name, int *rank, int *dtype, int *dsize, int *bounds, char *data, int *header, int *swap, int *ierr)
{
    int width, size;
    int r;
    char vname[32];
    int myrank,mydtype;
    int *mybounds;

    /* name */
    width=1;
    size=32;
    c_fbread (iunit,vname,&width,&size,swap,ierr);

    /* rank */
    width=sizeof(*rank);
    size=1;
    c_fbread(iunit,(char *)&myrank,&width,&size,swap,ierr);
    if ( myrank != *rank) {
        fprintf(stderr," error in c_fbread_record %s myrank != *rank %d %d \n", name, myrank , *rank);
        *ierr = -1;
        return;
    }

    /* dtype */
    width=sizeof(*dtype);
    size=1;
    c_fbread(iunit,(char *)&mydtype,&width,&size,swap,ierr);
    if ( mydtype != *dtype) {
        fprintf(stderr," error in c_fbread_record %s mydtype != *dtype %d %d \n", name, mydtype , *dtype);
        *ierr = -1;
        return;
    }

    /* bounds */
    width=4;
    size=2*(*rank);
    c_fbread(iunit,(char *)bounds,&width,&size,swap,ierr);

    /* header */
    width=4;
    size=512;
    c_fbread(iunit,(char *)header,&width,&size,swap,ierr);

    /* data */
    size = 1;
    for (r=0; r<*rank; r++) {
        size = size * (bounds[r*2+1] - bounds[r*2+0] + 1);
    }
    c_fbread(iunit,data,dsize,&size,swap,ierr);

}
