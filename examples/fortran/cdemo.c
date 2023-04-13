
#include <stdio.h>
#include "cdemo.h"

int main()
{
   float a, b, c;
   spcomplex cproduct, csum, soln[2];
   dpcomplex dsoln[2];
   dpcomplex dproduct;

   while (scanf("%f,%f,%f", &a, &b, &c) == 3) {

	qsolve(&a, &b, &c, soln);
	printf("Coefficients: (%8.4f, %8.4f, %8.4f)\n",a,b,c);
	printf("Root #1     : (%8.4f, %8.4f i)\n", soln[0].r, soln[0].i);
	printf("Root #2     : (%8.4f, %8.4f i)\n", soln[1].r, soln[1].i);

	dsoln[0].r = soln[0].r;
	dsoln[0].i = soln[0].i;
	dsoln[1].r = soln[1].r;
	dsoln[1].i = soln[1].i;

	dcmultsfwrap(&dproduct, dsoln, dsoln+1);
	printf("D-Multiplied: (%8.4f, %8.4f i)\n",dproduct.r, dproduct.i);

	cmultsfwrap(&cproduct, soln, soln+1);
	printf("C-Multiplied: (%8.4f, %8.4f i)\n",cproduct.r, cproduct.i);

	c_cadd(soln[0], soln[1], &csum);
	printf("C-Added     : (%8.4f, %8.4f i)\n\n",csum.r, csum.i);
   }

   return 0;
}
