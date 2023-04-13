typedef struct { float r, i; } spcomplex;	/* names chosen to minimize */
typedef struct { double r, i; } dpcomplex;	/* conflict w/ C99 complex  */
#define FTN_STR char*
#define FTN_STR_ARR char*
#define HAVE_FORTRAN_CODE 1
#define NDIMS(dims_array)  sizeof((dims_array)) / sizeof(SLindex_Type)
