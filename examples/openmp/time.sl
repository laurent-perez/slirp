import("par");

define fabs(x) { abs(x); }
define sum_cs(x) { cos(x) + sin(x); }
define psum_cs(x) { pcos(x) + psin(x); }
define Strlen(x) { array_map(Integer_Type, &strlen, x); }
define pStrlen(x) { pstrlen(x); }

private define clock(x, name)
{
   variable N = 10;
   variable ftime = 0, ptime = 0, pname = "p" + name;
   variable func =  __get_reference(name), pfunc = __get_reference(pname);

   () = (@func)(x[[1:10]]);	% run once, to cache
   () = (@pfunc)(x[[1:10]]);

   loop (N) {
      tic;  () = (@func)  (x);  ftime += toc;
      tic;  () = (@pfunc) (x);  ptime += toc;
   }
   () = printf("%2.6f   %2.6f\t",ftime/N, ptime/N);
}

vmessage("\n# Comparison of intrinsic S-Lang functions with vectorized");
vmessage("# and OpenMP-parallelized implementations generated by SLIRP.");
vmessage("# Set OMP_NUM_THREADS=<n> on Solaris & possibly multicore systems.");
vmessage("# OMP_NUM_THREADS=<%S>\n", getenv("OMP_NUM_THREADS"));

_auto_declare = 1;
sizes = [20, 250, 500, 1000, 3000, 5000, 10000, 2e4, 5e4, 3e5, 7e5, 3e6, 1e7, 2e7];
mathfuncs =  ["sin", "cos", "log", "fabs", "sqrt", "sum_cs"];
funcs = [mathfuncs, "strlen"];

() = printf("# Array Sizes\t%s\n", strjoin( array_map(String_Type,
		&sprintf, "%8s   %8s\t", funcs, "p" + funcs), ""));

foreach(sizes)
{
   size = int(());
   x = [PI : PI*100: PI/size*99];
   () = printf("%8d\t",size);

   foreach(mathfuncs) {
	name = ();
	clock(x, name);
   }

   % String processing is slow in native S-Lang; do it only for smaller arrays
   if (size <= 5e5)
	clock( array_map(String_Type, &string, x), "Strlen");
   else
	() = printf("%2.6f   %2.6f\t", 0, 0);

   () = printf("\n");
   () = fflush(stdout);
}