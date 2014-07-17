/*ams #include <malloc.h> ams*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// A quicksort implementation for intergers with Fortran bindings.
// The Key can be (long long- integer*8) or (int - integer*4) and
// it is sorted in place. The simple interface (qsort0) has just
// the key and its length as arguments.


// This function swaps i-j items of the key and of any ancillary data contained
// in the b array. n is the number of columns in b and its sign
// determines which dimension holds the columns.


void QSswap(long long a[], int b[], int i, int j, int m, int n)
{
   if(i!=j) {

     int s, k;
     long long l;

     l=a[i]; a[i]=a[j]; a[j]=l;

     if     (n>0) {
       for(k=0;k< n;k++) {s=b[n*i+k]; b[n*i+k]=b[n*j+k]; b[n*j+k]=s;}
     } 
     else if(n<0) {
       for(k=0;k<-n;k++) {s=b[i+m*k]; b[i+m*k]=b[j+m*k]; b[j+m*k]=s;}
     }

   }
}

// The basic quicksort function.


void QuickSort(long long a[], int b[], int l, int r, int m, int n)
{
  int len=r-l+1;

  if (len<=1) {
    return;
  } 
  else if (len==2) {
    if (a[l]>a[r]) QSswap(a,b,l,r,m,n);
    return;
  }
  else {
    int       j = r;
    int       i = l-1;
    long long v = a[r];

    for(;;)  {
      while(a[++i]<v && i<r);
      while(a[--j]>v && j>l);
      if (j<=i) break;
      QSswap(a,b,i,j,m,n);
    }

    QSswap   (a,b,i  ,r,m,n);
    QuickSort(a,b,l  ,j,m,n);
    QuickSort(a,b,i+1,r,m,n);
  }
}






// FORTRAN INTERFACES

void QSORT0(long long a[], int *r) {
  int *b=NULL;
  (void)QuickSort(a,b,0,*r-1,*r,0);
}

void QSORTL(long long a[], int b[], int *r, int *n) {
  (void)QuickSort(a,b,0,*r-1,*r,*n);
}

// For now the versions with 32-bit keys just make long long copies
//  and use the versions with 64-bit keys. This is inefficient, but simnple.

void QSORTS (int a[], int b[], int *r, int *n) {
  int i;
  long long *c;

  // printf(" In qsorts %d %d %d \n",sizeof(int),sizeof(long),sizeof(long long));

  c=(long long *)malloc(*r*sizeof(long long));
  if(c==NULL){printf("Allocation error in sort\n");(void)exit(1);}

  for(i=0;i<*r;i++) c[i] = (long long)a[i];

  //  printf(" =====>> Calling quicksort %d %d \n",*r, *n);

  (void)QuickSort(c,b,0,*r-1,*r,*n);

  //  printf(" Copying back \n");

  for(i=0;i<*r;i++) a[i] = (int)c[i];

  (void)free(c);
}

// Extra aliases for other loaders

void qsort0 (long long a[],          int *r        ) { (void)QSORT0(a  ,r  ); }
void qsortl (long long a[], int b[], int *r, int *n) { (void)QSORTL(a,b,r,n); }
void qsorts (int       a[], int b[], int *r, int *n) { (void)QSORTS(a,b,r,n); }

void QSORT0_(long long a[],          int *r        ) { (void)QSORT0(a  ,r  ); }
void QSORTL_(long long a[], int b[], int *r, int *n) { (void)QSORTL(a,b,r,n); }
void QSORTS_(int       a[], int b[], int *r, int *n) { (void)QSORTS(a,b,r,n); }

void qsort0_(long long a[],          int *r        ) { (void)QSORT0(a,  r  ); }
void qsortl_(long long a[], int b[], int *r, int *n) { (void)QSORTL(a,b,r,n); }
void qsorts_(int       a[], int b[], int *r, int *n) { (void)QSORTS(a,b,r,n); }


