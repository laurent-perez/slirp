private variable num_iterations_per_test = 10;

define benchmark_make_strings(size) % {{{
{
   return array_map(String_Type, &sprintf, "str%d", [1:size]);
} % }}}

private define vectorized_strlen_intrinsic(strings) % {{{
{
   return array_map(Int_Type, &strlen, strings);
} % }}}

define benchmark_set_num_iterations(n) % {{{
{
   num_iterations_per_test = n;
} % }}}

define benchmark() % {{{
{
   variable input1, input2, func;
   variable runtimes = Double_Type[0], niters = num_iterations_per_test;

   if (_NARGS == 2)
	(func, input1) = ();
   else
	(func, input1, input2) = ();

   if (func == "strlen")
	func = &vectorized_strlen_intrinsic;
   else 
	func = __get_reference( strtrim(func) );

   
   if (_NARGS == 2) {
	() = (@func) (input1);			% run once to warm cache
	loop(niters) {
	   tic();
	   () = (@func) (input1);
	   runtimes = [runtimes, toc() ];
	}
   }
   else {
	() = (@func) (input1);			% run once to warm cache
	loop(niters) {
	   tic();
	   () = (@func) (input1, input2);
	   runtimes = [runtimes, toc() ];
	}
   }

   variable biggest = max(runtimes);
   variable smallest = min(runtimes);

   return (sum(runtimes) - biggest - smallest) / (niters - 2);
} % }}}

provide("benchmark");
