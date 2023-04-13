
#ifndef __PI_HH__
#define __PI_HH__

#include "computation.hh"

class Pi : public Computation 
{
	long _nintervals;

   public:

	Pi() : Computation(0x002, "Pi"), _nintervals((long)1e7)	{ }
	Pi(unsigned long n) : Computation(0x002, "Pi"), _nintervals(n) { }
	Pi(Pi& p) : Computation(p), _nintervals(p._nintervals) {}
		        
	~Pi()		{ if (_verbose) cout << "Pi destructor\n"; }
	double	compute(void);
};

#endif
