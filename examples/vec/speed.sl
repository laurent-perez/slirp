% Example:
%
%	slsh <this_script> sqrt  1000000
%	slsh <this_script> vsqrt 1000000
%
% Compares native S-Lang sqrt to SLIRP-vectorized version, using 1E6 elements
% For accuracy, both slsh and module must have the same optimzation levels.

if (__argc < 3)
   usage("%s funcname array_size", path_basename(__argv[0]));

import("vec");
_auto_declare = 1;
funcname = __argv[1];
func = __get_reference(funcname);
arrsize = int(atof(__argv[2]));		% atoi doesn't exist in SLang1
in = [1.0:arrsize*1.0+1];
l = length(in);

tic();
out1 = @func(in);
vmessage("%s time (%d elems) = %f", funcname, l, toc());
