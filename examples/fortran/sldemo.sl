set_import_module_path( strcat(".", ":", get_import_module_path()));

import("fsubs");
() = evalfile("../examples-common");

variable line, a, b, c, solution;

while (-1 != fgets( &line, stdin)) {
   if (sscanf(line, "%f,%f,%f", &a, &b, &c) != 3) break;
   variable solution = qsolve(a, b, c);
   printf("Coefficients: (%8.4f, %8.4f, %8.4f)\n",a,b,c);
   printf("Root #1     : (%8.4f, %8.4f i)\n", Real(solution[0]),
							Imag(solution[0]));
   printf("Root #2     : (%8.4f, %8.4f i)\n", Real(solution[1]),
							Imag(solution[1]));

   variable csum, product = dcmult(solution[0], solution[1]);
   printf("D-Multiplied: (%8.4f, %8.4f i)\n", Real(product), Imag(product));

   product = cmult(solution[0], solution[1]);
   printf("C-Multiplied: (%8.4f, %8.4f i)\n", Real(product), Imag(product));

   % This example intentially avoids using a complex *OUTPUT type mapping,
   % (which would return csum on the stack), so as to better exercise SLIRP
   c_cadd(solution[0], solution[1], &csum);
   printf("C-Added     : (%8.4f, %8.4f i)\n\n", Real(csum), Imag(csum));
}

exit(0);
