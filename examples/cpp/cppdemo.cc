#include "compat.hh"
#include "pi.hh"
#include "vec.hh"
#include "err.hh"			// included just to verify class

int main()
{
   Pi *p = new Pi((unsigned long)1e6);
   validate_state(p, Computable);
   invoke_zero_arg_methods(p);
   delete p;

   Pi p2(100);
   invoke_zero_arg_methods(&p2);

   long id = -999;
   p2.id_r(id);
   if (id != p2.id())
	cout << "id_r(long&) is broken, it returned <" << id << ">" <<endl;

   p = new Pi(p2);
   invoke_zero_arg_methods(p);
   delete p;

   short s = 1000;
   int   i = 33000;
   double d = 3000000000.;
   long l = -66666;
   unsigned long ul = 133332;

   output(s);
   output(i);
   output((int)l);
   output((short)ul);
   output(d);

   double darr[] = {1.1, 2.2, 3.3, 4.4};
   output(darr, sizeof(darr) / sizeof(double) );

   double *da = make_array_d(3);
   output(da, 3);
   delete [] da;

   change(d);
   output(d);

   ProcessGlobal *pg = new ProcessGlobal();
   pg->report();

   Vec v1(3,4);
   Vec v2(3,4);
   double dp = v1.dot(v2);
   if (dp != 25)
	cout << "v1 dot v2 == " << dp << ", not 25!" << endl;
}
