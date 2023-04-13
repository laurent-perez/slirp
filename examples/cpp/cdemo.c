
#include <limits.h>
#include <float.h>
#include <stdio.h>
#include "cdemo_glue.h"

int main (int argc, char **argv)
{
   /* Pure C version of cppdemo, which should produce exactly the same output */
   short s = 1000;
   int   i = 33000;
   double d = 3000000000.;
   long l = -66666;
   unsigned long ul = 133332;
   double darr[] = {1.1, 2.2, 3.3, 4.4};
   double *darr2 = c__make_array_d(3);
   long id_r = -999;

   void *p = c__Pi_new_L((unsigned long)1e6);
   void *p2;

   (void) argc; (void) argv;
   c__validate_state(p, 0);

   c__invoke_zero_arg_methods(p);
   c__Computation_delete(p);

   p = c__Pi_new_L((unsigned long)100);
   c__invoke_zero_arg_methods(p);
   if (c__Computation__state_get(p) != 1)
	printf("Pi(100) should be in 'Computed' state, but is not");

   c__Computation__state_set(p, 0);
   if (c__Computation__state_get(p) != 0)
	printf("Pi(100) should be in 'Computable' state, but is not");

   c__Computation_id_r(p, &id_r);
   if (id_r != c__Computation_id(p))
	printf("id_r(long&) cfront wrapper broken, it returned <%ld>\n",id_r);

   p2 = c__Pi_new_o(p);
   c__invoke_zero_arg_methods(p2);
   c__Computation_delete(p);

   c__output_h(s);
   c__output_i(i);
   c__output_i(l);
   c__output_h(ul);
   c__output_d(d);
   c__output_nL(darr, sizeof(darr) / sizeof(double) );
   c__output_nL(darr2, 3);

   c__change(&d, 999.999);
   c__output_d(d);

   { 
	void *process_global = c__ProcessGlobal_new();
	c__ProcessGlobal_report(process_global, "ok!");
   }

   {
	void *v1 = c__Vec_new_dd(3,4), *v2 = c__Vec_new_o(v1);
	double dp = c__Vec_dot(v1, v2);
	if (dp != 25)
	   printf("v1 dot v2 == %f, not 25!\n", dp);
   }

   c__Computation_delete(p2);

   return 0;
}
