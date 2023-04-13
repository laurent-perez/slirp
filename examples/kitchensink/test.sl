
import("ksink");
() = evalfile("./example");

dump_stack("Examples Script");
print("*** Test: examples done, now executing more stringent cases ***\n\n");

ksink_close(open("./example.sl", O_RDONLY));
if (ksink_ftell(fopen("./example.sl", "r")) != 0)
   print("ksink_ftell() did not return 0 for file just fopen()-ed!");

variable size, nlinks, fp;
(fp, size, nlinks) = ksink_fopen("./example.sl", "r");

if (orelse {typeof(size) != ULong_Type} {typeof(nlinks) != UShort_Type})
   print("ksink_fopen() did not return correctly typed value sequence!");

if (ksink_ftell(fp) != 0)
   print("ksink_ftell() did not return 0 for file just ksink_fopen()-ed!");

check_ignored("ksink_ignorable_func");
check_ignored("KSINK_IGNORABLE_MACRO");
check_ignored("KSINK_IGNORABLE_MACRO2");
check_ignored("KSINK_IGNORABLE_INT");
check_ignored("KSINK_IGNORABLE_INT2");
check_ignored("BAD1");
check_ignored("BAD2");

if (GOOD1 != 999) print("GOOD1 macro not wrapped properly!");
if (orelse {typeof(GOOD2) != String_Type} {GOOD2 != "HI!"})
				print("GOOD2 macro not wrapped properly!");
check_ignored("GOOD3");

variable kds = ksink_dummy_new();
!if (ksink_dummy_ok(kds))
	print("KDummy struct not wrapped properly!");

ksink_toggle_error_hook();

% Verify pointer casting equivalence
_try(&ksink_print_array_i, 3, ksink_voidstar_echo(opaque_int_array));
_try(&ksink_print_array_s, 3, ksink_voidstar_echo(opaque_string_array));

() = ksink_subtract_ptrs(opaque_string_array, opaque_int_array);
() = ksink_sizeof_int();

% The next two should pass: the first because it's correct usage, and
% the second because -refscalars is specified at SLIRP invocation time
variable od = ksink_datum_new("test",3.2), f = 99, copy;
_try(&ksink_print_array_datum,1, [od]);
_try(&ksink_print_array_f,f);

if ( typeof(ksink_datum_copy(od)) != KDatum_Type)
   print("ksink_datum_copy did not return a value of type KDatum_Type!");

% Verify pointer type checking:  every remaining test should signal an error
print;
_try(&ksink_print_array_f,opaque_int_array);
_try(&ksink_print_array_f,1,"string");
_try(&ksink_print_array_f,NULL);
_try(&ksink_print_array_i,1,12.5);
_try(&ksink_print_array_datum,opaque_int_array);

_try(&typecast,opaque_int_array,opaque_ptr);
_try(&typecast,opaque_int_array,uchar_ptr);
_try(&typecast,opaque_int_array,string_ptr);

_try(&typecast,opaque_string_array,float_ptr);
_try(&typecast,opaque_string_array,double_ptr);

_try(&typecast,opaque_datum_array,long_ptr);
_try(&typecast,opaque_datum_array,ushort_ptr);

_try(&ksink_voidstar_echo, 11.2);

_try(&ksink_print_array_s, opaque_int_array);

_try(&ksink_print_array_datum, 1, od);

% The next call attempts a NULL ptr reference, since the public S-Lang C api
% doesn't provide a way of dereferencing (obtaining the value of) a S-Lang ref
_try(&ksink_print_array_datum, 1, &od);

% This should throw an array size mismatch error
_try(&ksink_equals_array_d, [10:20], [99:100] );
_try(&ksink_arg_dropper_2, 1,2,3,4);
