
set_slang_load_path("..:"+get_slang_load_path());
set_slang_load_path("../..:"+get_slang_load_path());

require("benchmark");

private define emit() % {{{
{
   variable args = __pop_args(_NARGS);
   () = printf(__push_args(args));
   () = fflush(stdout);
} % }}}

variable funcs_with_two_pars = ["hypot"], factor = 1;
variable arg = 1, niters=20, module = "vec", prefix = "v", nargs = __argc;

if (__argv[0] == "isis") {
   arg = 2;
   nargs--;
}

if (nargs < 2) {
   emit("<application> perform.sl [options] function_name\n"+
	"Options:\n"+
	" -p                         use parallel par-module.so\n"+
	"                            (default: vec-module.so)\n"+
	" -n number_of_iterations    change the default from 20\n"+
	" -s size_factor             scale the array sizes used\n"+
	" -l script                  load script to define funcs that will\n"+
	"                            be tested, instead of *-module.so\n");
   exit(1);
}

while (__argv[arg][0] == '-') { 
   if (__argv[arg] == "-p") {		% openmp-parallel module
	module = "par";
	prefix = "p";
   }
   else if (__argv[arg] == "-n") {	% number of iterations
	niters = atol(__argv[arg+1]);
	arg++;
   }
   else if (__argv[arg] == "-s") {	% multiplier for array sizes
	factor = atol(__argv[arg+1]);
	arg++;
   }
   else if (__argv[arg] == "-l") {	% load slang script
	() = evalfile(__argv[arg+1], "Global");
	module = NULL;
	arg++;
   }
   if (arg+1 >= __argc) break;
   arg++;
}

variable func = __argv[arg];

if (module != NULL) {
   import(module);
   variable funcs = [ func, prefix + func];
}

benchmark_set_num_iterations(niters);

%variable sizes = int([100, 1000, 10000, 100000, 1000000, 10000000] * factor);
variable sizes = int ([250, 500, 1000, 3000, 5000, 10000, 2e4, 5e4, 3e5, 7e5, 3e6, 1e7, 2e7] * factor);

emit("# SLIRP vectorization benchmark data\n");
emit("# Generated: %s\n", time());
emit("# Command:   %s\n", strjoin(__argv, " "));
emit("# Number of Iterations (func invocations) per array size: %S\n", niters);
emit("# OMP_NUM_THREADS=%S\n", getenv("OMP_NUM_THREADS") );
%hinfo = uname();
%emit("# System:    %S (%S %S)\n\n", hinfo.nodename,hinfo.machine,hinfo.sysname);


emit( "# Elements");
foreach(funcs) {
   func = ();
   () = printf("\t%7s", func);
}
emit("\n\n");

variable npars = any(func == funcs_with_two_pars) + 1;

foreach (sizes)
{
   variable size = (), input;

   emit("%10d\t", size);

   foreach(funcs) {
	func = ();

   if (is_substr(func, "strlen")) {
	if (size > 499999) {		% string generation is just too slow
	   emit("0");
	   continue;
	}
	input = benchmark_make_strings(size);
	}
   else
	input = [ 1.0 : size*1.0 + 1];

   if (npars == 2)
	emit("%4.3e\t", benchmark(func, input, input));
   else
	emit("%4.3e\t", benchmark(func, input));
   }

   emit("\n");
}
