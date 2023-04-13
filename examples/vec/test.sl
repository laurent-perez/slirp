import("vec");
() = evalfile("../examples-common");

% Vectorized C mathematical functions {{{
variable in  = PI / 2;
if (vsin(in) != sin(in)) print("vsin(scalar) failed!");

variable result = vsin(Double_Type[0]);
if (length(result) != 0) print("vsin(empty_array) failed");

in = [1.0: 10000.0];
if (array_neqs(vsin(in), sin(in)) ) print("vsin(vector) failed!");

in = Double_Type[5,3] + PI / 2;
variable out = sprintf("%S", vsin(in));
if (out != "Double_Type[5,3]")  print("vsin(Double_Type[5,3]) failed!");

in = _reshape([1,1,1,2,2,2,3,3,3,4,4,4], [2,2,3]);
variable in2 = _reshape([4,4,4,5,5,5], [2,3]);
% mult expects scalars, so even though in1, in2, and 3 are of unequal length,
% these mult calls succeed by iterating over the first elem in the 2nd arg
out = mult(in, in2);
if (array_neqs(out, 4 * in)) print("mult(longer_array,shorter_array) 1 failed");
out = mult(in, 3);
if (array_neqs(out, 3 * in)) print("mult(array, scalar) 2 failed");
% }}}

% Vectorized text functions {{{
if (tag(1) != "tag-1") print("tag(1) failed!");
variable tag_123 = ["tag-1", "tag-2", "tag-3"];
if (array_neqs(tag([1,2,3]), tag_123)) print("tag([1,2,3]) failed!");

out = tag( _reshape( [1,2,3,1,2,3], [2,3]));
if (array_neqs(out[0,*], tag_123)) print("tag(Int_Type[0,*]) failed!");
if (array_neqs(out[1,*], tag_123)) print("tag(Int_Type[1,*]) failed!");

out = vstrlen(["one","TOOO","three"]);
if (array_neqs(out, [3, 4, 5])) print("strlen(String_Type[3]) failed!");
% }}}

% 1D array vectorizations % {{{
variable OneTwoThree = [1., 2., 3.], Ones = [1., 1., 1.], Nines = [9., 9., 9.];
out = vmult( OneTwoThree, [2.0,3.0,4.0]);
if (array_neqs(out, [2.0, 6.0, 12.0])) print("vmult attempt 1 failed");

in = Double_Type[5,3];
in[0,*] = 1.0; in[1,*] = 2.0; in[2,*] = 3.0; in[3,*] = 4.0; in[4,*] = 5.0;
out = vmult(OneTwoThree, in);
if (array_neqs(out[0,*], OneTwoThree))   print("vmult(1D,2D) [0] failed");
if (array_neqs(out[1,*], 2*OneTwoThree)) print("vmult(1D,2D) [1] failed");
if (array_neqs(out[2,*], 3*OneTwoThree)) print("vmult(1D,2D) [2] failed");
if (array_neqs(out[3,*], 4*OneTwoThree)) print("vmult(1D,2D) [3] failed");
if (array_neqs(out[4,*], 5*OneTwoThree)) print("vmult(1D,2D) [4] failed");

variable in3D = Double_Type[2,5,3];
in3D[0, *, *] = in/5;
in3D[1, *, *] = 2*in;
out = vmult(Nines, in3D);
if (array_neqs(out[0,*,*], in/5*9))   print("vmult(1D,3D) [0] failed");
if (array_neqs(out[1,*,*], in*2*9))   print("vmult(1D,3D) [1] failed");

variable arr2d_1 = _reshape( [5,5,5,100,100,100], [2,3]);
variable arr2d_2 = _reshape( [100,100,100, 5, 5, 5], [2,3]);
out = vmult(arr2d_1, arr2d_2);
if (array_neqs (out, Double_Type[2,3] + 500)) print("vmult(2D,2D) failed");

% Raise some signals: array/scalar mismatch errors and Usage statement(s)
_try(&vmult, 9, [3.0, 4.0]);
_try(&vmult, [Nines, 9.9], Ones);
_try(&vmult, [9.9, 9.9], Ones);
_try(&vmult, in, 5);
_try(&vmult);
% }}}

% ND array vectorizations {{{
variable in1 = [1.,1.,1.,1.], in2 = [4.,4.,4.,4.], out = [5.,5.,5.,5.];
reshape(in1, [2,2]); reshape(in2, [2,2]); reshape(out, [2,2]);
add2d(in1, in2);
if (array_neqs(in1,out))   print("add2d failed");

in3D = Double_Type[3,2,2];
in3D[0,*,*] = in1;
in3D[1,*,*] = in1*2;
in3D[2,*,*] = in1*3;
add2d(in3D, in2+1);
if (array_neqs(in3D[0,*,*], in1*2))   print("add2d 2 [0] failed");
if (array_neqs(in3D[1,*,*], in1*3))   print("add2d 2 [1] failed");
if (array_neqs(in3D[2,*,*], in1*4))   print("add2d 2 [2] failed");

% Show that a 1D vector of MxN elems can be used in place of ND MxN array
in3D[0,*,*] = in1;
in3D[1,*,*] = in1*10;
in3D[2,*,*] = in1*20;
add2d(in3D, [5,5,5,5]);
if (array_neqs(in3D[0,*,*], in1*2))   print("add2d 3 [0] failed");
if (array_neqs(in3D[1,*,*], in1*11))  print("add2d 3 [1] failed");
if (array_neqs(in3D[2,*,*], in1*21))  print("add2d 3 [2] failed");

% Now show how DIM1, DIM2, etc can be used to dimensionalize a 1D vector
in3D = _reshape([1,1,1,1,1,1,1,1,1, 9,9,9,9,9,9,9,9,9], [3,2,3]);
out = in3D - 1;
sub1_2d(in3D);
if (array_neqs(in3D,out)) print("sub1_2d failed");

_try(&add2d, [1,1,1,1], [2,2,2]);	% this will raise a signal
% }}}

% Vectorized C++ methods  {{{

variable p = Pi_new(100000), pi = Computation_compute(p), inout = @OneTwoThree;
Computation_mult(p, inout, 3);
if (array_neqs(inout, pi * OneTwoThree)) print("Pi_mult 1 failed");

inout = Double_Type[2,3];
inout[0,*] = OneTwoThree;
inout[1,*] = 2 * OneTwoThree;

output(inout);

Computation_mult(p, inout, 3);
if (array_neqs(inout[0,*], pi * OneTwoThree)) print("Pi_mult 2 [0] failed");
if (array_neqs(inout[1,*], 2 * pi * OneTwoThree)) print("Pi_mult 2 [0] failed");

variable parr = Pi_new([100, 10000]);
if (length(parr) != 2 or typeof(parr) != Array_Type or _typeof(parr)!= Pi_Type)
   print("Pi_new([100, 10000] failed");
% }}}

% Vectorized FORTRAN code {{{
#ifeval is_defined("fcadd")
out = fadd(Nines, Ones);
if (array_neqs(out, Nines + Ones)) print("fadd failed");

variable cmplx1 =  1 + 1.4142i, cmplx2 =  -cmplx1;
variable cmplx3 =  2, cmplx4 =  -cmplx3;
variable cmplx5 =  -1 + 1i, cmplx6 =  -cmplx5;

if (fcadd(cmplx1, cmplx2) != 0.0) print("fcadd(cmplx1, cmplx2) failed");
if (fcadd(cmplx3, cmplx4) != 0.0) print("fcadd(cmplx3, cmplx4) failed");
if (fcadd(cmplx5, cmplx6) != 0.0) print("fcadd(cmplx5, cmplx6) failed");

out = fcadd([cmplx1, cmplx3, cmplx5], [cmplx2, cmplx4, cmplx6]);
if (length(where(out != 0.0))) print("vectorized fcadd failed");
#endif
% }}}

% Usage messages exhibiting vectorization (or lack of) {{{
_try(&Computation_set_verbose);		
_try(&stupid);				% too many arguments to vectorize
_try(&add2dp);				% arrays of pointers not vectorized
% }}}

% Vectored opaque types % {{{
variable n1 = Number_new(1), n2 = Number_new(2);
Number_set(n1, 333);
if (Number_get(n1) != 333) print("Number_get(scalar) failed");

Number_set([n1, n2], [99, 999]);
if (array_neqs(Number_get([n1,n2]),[99,999])) print("Number_set(array) failed");

Number_set( n2, 5);
Number_set([n1, n2], [7,8,9]);
% Verify that only 1st elem of shorter arr is used for each iter of longer arr
if (array_neqs(Number_get([n1,n2]),[9,5])) print("Number_set(array2) failed");

variable dp1 = make2dp(3), dp2 = make2dp(5);
print("%S",dp1);
add2dp(dp1, dp2);
print_and_free_2dp(dp1);

variable dparr = make2dp([10,20]);
add2dp(dparr, dp2);
print_and_free_2dp([ dp2, dparr ]);

_try(&add2dp);				% emit usage
_try(&make2dp);				% emit usage
% }}}
