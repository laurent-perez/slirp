
#include "pi.hh"

double Pi::compute(void)
{
   double x, dx = 1.0 / _nintervals, sum = 0.0;

   for (long i = 0; i < _nintervals ; i++) {
	x = dx * ((double)i - 0.5);
	sum += 4.0/(1.0 + x*x);
   }
   sum *= dx;

   _state = Computed;
   return sum;
}
