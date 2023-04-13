% Example:
%
%	slsh <this_script> sqrt  1000000
%	slsh <this_script> vsqrt 1000000
%
% Compares native S-Lang sqrt to SLIRP-vectorized version, using 1E6 elements
% For accuracy, both slsh and module must have the same optimzation levels.

import("vec");
require("benchmark");
_auto_declare = 1;

if (__argc < 3)
   usage("%s funcname array_size", path_basename(__argv[0]));

funcname = __argv[1];
arrsize = int(atof(__argv[2]));		% atoi doesn't exist in SLang1

switch(funcname)
   { case "strlen" or case "vstrlen" : in = benchmark_make_strings(arrsize); }
   { in = [1.0:arrsize*1.0+1];}

() = printf("%f\n", benchmark(funcname, in));
