#include "../../src/config.h"
#include "../../src/lib/fortran.h"
#include "csubs.h"

#define qsolve FC_FUNC(qsolve, QSOLVE)
#define dcmultsfwrap FC_FUNC(dcmultsfwrap, DCMULTSFWRAP)
#define cmultsfwrap FC_FUNC(cmultsfwrap, CMULTSFWRAP)

extern void qsolve (float *a, float *b, float *c, spcomplex *solution);
extern void dcmultsfwrap (dpcomplex *ret, dpcomplex *a, dpcomplex *b);
extern void cmultsfwrap (spcomplex *ret, spcomplex *a, spcomplex *b);
