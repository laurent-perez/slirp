//
// SLIRP C++ example
//
#ifndef __COMPUTATION_HH__
#define __COMPUTATION_HH__

#include "compat.hh"

typedef enum { Computable, Computed } State;

class Computation
{

  public:

	Computation(long ID, const char* NAME)	{ _id = ID;
	   					  _name = NAME;
	   					  _state = Computable;
						  _verbose = 1;
						}

	virtual ~Computation()	{ if (_verbose) cout <<
	   				"Computation destructor" <<endl; }

	virtual double	compute(void)	= 0;
	const char*	name(void)			{ return _name; }
	long 		id(void)			{ return _id; }
	void		id_r(long & out)		{ out = _id; }
	void		set_verbose(unsigned char v)	{ _verbose = v; }

	void		mult(double *array, unsigned long size);

#ifdef SILLY_DO_NOT_DEFINE
	void		silly(int i, double d)		{ (void) i; (void) d; }
#endif

	State		_state;

  protected:

	unsigned char	_verbose;
	long		_id;
	const char*	_name;
};

class ProcessGlobal
{
  public:

	ProcessGlobal()		{}
	static void report(string msg="ok!")	{ cout << 
				"ProcessGlobal::report(): " << msg << endl;}
};

extern void validate_state(Computation *c, State s=Computable);
extern void invoke_zero_arg_methods(Computation *c);
extern void output(short s);
extern void output(int i);
extern void output(double d=111.111);
extern void output(unsigned long size, double *d1, double *d2 = NULL);
extern void output(double *d, unsigned long size = 1);
extern void output(int *d, unsigned long size = 1);
extern void change(double & d , double newval=999.999);
extern bool istrue(bool b);

extern double* make_array_d(unsigned long size = 1);

#endif
