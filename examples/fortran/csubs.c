#include "../../src/lib/fortran.h"

/* C version of adding 2 single-precision complex numbers */
void c_cadd(spcomplex a, spcomplex b, spcomplex *result)
{
   result->r = a.r + b.r;
   result->i = a.i + b.i;
}
