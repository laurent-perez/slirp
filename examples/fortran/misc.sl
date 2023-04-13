set_import_module_path( strcat(".", ":", get_import_module_path()));

import("fsubs");

#iffalse
initcomm("testing123");
$1 = get_commblock("com5");
print ($1);
print ($1.strarr);
exit (1);
#endif

() = evalfile("../examples-common");

check_ignored("rzero");
_try(&imptest);			% emit usage statement to validate code
				% generated with implicitly typed args

_try(&mmult);			% emit usage to validate that FORTRAN
				% array dimensions are parsed correctly

if (6 != sub_multi(3, 2))	% ensure that sub_multi is usable as a function
   print("sub_multi was not properly wrapped/morphed to a function");

if (3.33e12F != paramtest())
   print("ugly parameter statements were not parsed correctly");

% Exercise multidimensional array transposition for FORTRAN calls
variable a = _reshape([1., 2,  3,  4,  5,  6, 7, 8],  [4,2]);
variable b = _reshape([60.,50, 40, 30, 20, 10], [2,3]);
variable c = Double_Type[4,3];

% FIXME: make result of mmult() an OUTPUT arg
mmult(a,b,c,4,2,3);
if (array_neqs(c, a # b)) print("mmult 1 failed, fortran transpose problem?");

% Next invocation shows that dimension args are optional
c[*] = 0;
mmult(a,b,c);
if (array_neqs(c, a # b)) print("mmult 2 failed, fortran transpose problem?");

% NB: this assumes the default behavior for CHARACTER args: the hidden
%     CHARACTER argument length is appended to the end of the arg list
if (216.0F != continuator(99.9F, 999.999,"floppy",3.0F))
   print("string passing did not work properly");

check_ignored("bad_outstrarr");

variable arr = outstrarr();
if (any(arr != "strout"))
   print("out string array parameter not handled properly");

instrarr(arr);
if (any(arr != "blah  "))
   print("in/out string array parameter not handled properly");

variable arr2 = outstrarr2();
if (sprintf("%S", arr2) != "String_Type[2,3]")
   print("2D string array output: dimensions are incorrect");

arr = array_map(String_Type, &sprintf, "%S%d", outstrarr(), [1,3,5,2,4,6]);
arr2 = _reshape(arr2, [6]);
if (any(arr != arr2))
   print("2D string array output: content is incorrect");

if (rfirstlast([99,2,3,5,101]) != 200)
   print("automatic optional/elision of array arguments not working");

% Common block tests  {{{

variable names = get_commblock_list();
if (length(where(names[array_sort(names)] !=
	["_BLNK_", "com1", "com3", "com4", "com5", "com_block2_", "inct1com"])))
   print("Incorrect number or names of common blocks wrapped");


variable stringval = "StringCommon2";
initcomm(stringval);
arr = comtest;
if (any(["one       ", "two       ",] != arr))
   print("OUTPUT character array arg sized from COMMON block not working");

private define check_comm(type, value, block, var, kind, dims) % {{{
{
   block = get_commblock(block);
   variable got = get_commblock_value(block, var);

   if (typeof(got) != type)
	vmessage("common block: %s value not returned as %S", kind, type);

   if (type == Array_Type) {
	if (any( _reshape(got, dims) != value))
	   print("common block array value mismatch");
	got = 3;
	if (got != 3)
	   print("could not reassign variable containing common block array");
	return;
   }
   if (got != value)
	vmessage("common block: %s value does not match %S", kind, value);

} % }}}

check_comm(Float_Type, 9.9F, "com1", "rcom1", "single-precision real",);
check_comm(Integer_Type, 2, "com1", "icom1", "integer",);
check_comm(String_Type, stringval, "com_block2_","scom2", "string",);
check_comm(Double_Type, 333.333F, "com_block2_","rcom2", "double precision",);
check_comm(Array_Type, [1,3,5,2,4,6], "com3", "icom3arr", "integer array", [6]);

% Direct assignment to a common block array element
get_commblock("com3").icom3arr[1,1] = 99;
check_comm(Array_Type,[1,3,5,2,99,6],"com3","icom3arr","int array assign",[6]);

% Test single prec Fortran complex <--> double prec S-Lang complex
check_comm(Complex_Type, 3+2i, "com4", "comp1", "single complex",);
get_commblock("com4").comp1 = -9 - 9i;
check_comm(Complex_Type, -9-9i, "com4", "comp1", "single complex assign",);

% Test double prec complex array element assignment
check_comm(Array_Type,[40+50i, 60+70i], "com4", "comp4","double complex",[2]);
get_commblock("com4").comp4[0] = 11.1i;
check_comm(Array_Type, [11.1i, 60+70i], "com4", "comp4",
					"double complex array assign", [2]);

% Show that String and single-prec complex arrays are read-only, because
% the S-Lang <--> Fortran remapping doesn't happen on element assignment
private define fail_array_assign(statement)
{
   eval(statement);
}
_try(&fail_array_assign, "get_commblock(\"com5\").strarr[0] = \"dummy\"");
_try(&fail_array_assign, "get_commblock(\"com4\").comp2[1] = 2-2i");
% }}}

% Alias tests (alternate 'entry' points)  % {{{

if (not is_defined("altcmult"))
   print("alternate entry point for cmult not defined");

if (not is_defined("altqsolve"))
   print("alternate entry point for qsolve not defined");

if (altfirstlast([99,2,3,5,101]) != 200)
   print("alternate entry point for rfirstlast did not work");

arr = altoutstrarr();
if (any(arr != "strout"))
   print("alternate entry point for outstrarr did not work");

% }}}
