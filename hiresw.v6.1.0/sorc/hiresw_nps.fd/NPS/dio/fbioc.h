
#ifdef _UNDERSCORE
#define c_endian      c_endian_
#define c_swap        c_swap_
#define c_fbopen      c_fbopen_
#define c_fbread      c_fbread_
#define c_fbread_4bytes      c_fbread_4bytes_
#define c_fbwrite     c_fbwrite_
#define c_fbclose     c_fbclose_
#define c_fbseekset   c_fbseekset_
#define c_fbseekcur   c_fbseekcur_
#define c_fbseek_record   c_fbseek_record_
#define c_fbseekend   c_fbseekend_
#define c_fbtell      c_fbtell_
#define c_fbsize      c_fbsize_
#define c_fbwrite_record c_fbwrite_record_
#define c_fbread_record c_fbread_record_
#endif

#ifdef _DOUBLEUNDERSCORE
#define c_endian      c_endian__
#define c_swap        c_swap__
#define c_fbopen      c_fbopen__
#define c_fbread      c_fbread__
#define c_fbread_4bytes      c_fbread_4bytes__
#define c_fbwrite     c_fbwrite__
#define c_fbclose     c_fbclose__
#define c_fbseekset   c_fbseekset__
#define c_fbseekcur   c_fbseekcur__
#define c_fbseek_record   c_fbseek_record__
#define c_fbseekend   c_fbseekend__
#define c_fbtell      c_fbtell__
#define c_fbsize      c_fbsize__
#define c_fbwrite_record c_fbwrite_record__
#define c_fbread_record c_fbread_record__
#endif

void c_is_little_endian(int *endian);
void c_swap(char *a, const int width, const int size);

void c_fbopen (int *iunit, char *fname, int *flen, char *mode, int *ierr);

void c_fbread_4bytes (int *iunit, char *buffer, int *ierr);

void c_fbread (int *iunit, char *buffer, const int *width, const int *size, const int *swap, int *ierr);

void c_fbwrite (int *iunit, char *buffer, const int *width, const int *size, const int *swap, int *ierr);

void c_fbclose (int *iunit, int *ierr);

void c_fbseekset (int *iunit, uint64_t *seek, int *ierr);
void c_fbseekcur (int *iunit, uint64_t *seek, int *ierr);
void c_fbseek_record (int *iunit, int *swap, int *ierr);
void c_fbseekend (int *iunit, uint64_t *seek, int *ierr);
void c_fbtell (int *iunit, uint64_t *size, int *ierr);
void c_fbsize (int *iunit, uint64_t *size, int *ierr);

void c_fbwrite_record (int *iunit, char *name, int *rank, int *dtype, int *dsize, int *bounds, char *data, int *header, int *swap, int *ierr);
void c_fbread_record (int *iunit, char *name, int *rank, int *dtype, int *dsize, int *bounds, char *data, int *header, int *swap, int *ierr);
