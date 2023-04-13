
import("ksink");
() = evalfile("../examples-common");

define do_sum(augend,addend)
{
   print("ksink_sum (%S, %S) = %S",augend,addend, ksink_sum(augend,addend));
}

define do_mult(op1, op2, func)
{
   variable result = @func(op1,op2);
   if (typeof(op1) == Integer_Type)
	result = int(result);

   print("%S (%S, %S) = %S", func, op1, op2, result);
}

define use_ksink_datum()
{
   print;
   variable datum = ksink_datum_new("TheLorax",999.999);
   print("ksink_datum_new() returned: <%S>", typeof(datum));
}

define map_voidstar(thing)
{
   variable vs = ksink_voidstar_echo(thing);
   print("ksink_voidstar_echo mapped <%S> type to <%S> type",thing,vs);
}


print;
do_sum(-1,1);
do_sum(55,45);
print;

do_mult(333,3,&ksink_mult);
do_mult(333,3,&ksink_mult2);	% Verify argmap applied to ksink_mult2

set_float_format ("%16.14f");
do_mult(PI/2,2, &ksink_mult);
do_mult(PI/2,2, &ksink_mult2);	% Verify argmap applied to ksink_mult2

variable i = 111;
ksink_set_ref_i(&i);
print("\nAfter ksink_set_ref_i(&i) the new value of i is: %d",i);

use_ksink_datum();

% Test that functions which return arrays of indeterminate size,
% e.g. int*, are automatically remapped to return opaque pointers
variable opaque_int_array = ksink_make_array_i();
print("ksink_make_array_i() returned: <%S>", typeof(opaque_int_array));

% Verify that such can be passed back to C routines, e.g. which accept int*
ksink_print_array_i(3, opaque_int_array);

% Similar test for opaque pointers to string arrays of indeterminate size
variable opaque_string_array = ksink_make_array_s();
print("ksink_make_array_s() returned: <%S>",typeof(opaque_string_array));
ksink_print_array_s(opaque_string_array);

ksink_print_array_s(["one","two","three"]);

variable sarray_2d = String_Type[2,3];
sarray_2d[0,*] = ["sarray_2d_elem11","sarray_2d_elem12","sarray_2d_elem13"];
sarray_2d[1,*] = ["sarray_2d_elem21","sarray_2d_elem22","sarray_2d_elem23"];
ksink_print_array_s(sarray_2d);

% And for opaque pointers to indeterminately sized arrays of opaque types
variable opaque_datum_array = ksink_make_array_datum();
print("ksink_make_array_datum() returned: <%S>", typeof(opaque_datum_array));
ksink_print_array_datum(3,opaque_datum_array);

print;
ksink_print_array_f([1.1, 2.2, 3.3, 4.4, 5.5]);

% Now show that -refscalars option allows scalars to be
% passed to functions expecting array arguments
print;
ksink_print_array_i(1, 999);
ksink_print_array_f(11.1);


print;
% Demonstrate use of enumeration value mnemonics
Print_Error(KSINK_GOOD);
Print_Error(KSINK_BAD);
Print_Error(KSINK_UGLY);

print;

% Demonstrate handling of void* args and return values
map_voidstar(opaque_int_array);
map_voidstar(opaque_string_array);
map_voidstar(opaque_datum_array);

variable nts = ksink_make_array_nts();
ksink_print_array_nts(nts);

ksink_int_accepter(101,202,303);

print;
variable j = -100;
print("Before ksink_swap_double(&i, &j): i=<%f> and j=<%f>", i,j);
ksink_swap_double(&i, &j);
print("After ksink_swap_double(&i, &j): i=<%f> and j=<%f>", i,j);
i = 999;
print("Before ksink_swap_double(i, &j): i=<%f> and j=<%f>", i,j);
ksink_swap_double(i, &j);
print("After ksink_swap_double(i, &j): i=<%f> and j=<%f>", i,j);
i = 333.333; j = 111.11;
print("Before ksink_swap_double(i, j): i=<%f> and j=<%f>", i,j);
ksink_swap_double(i, j);
print("After ksink_swap_double(i, j): i=<%f> and j=<%f>", i,j);

% Next few examples exercise #argmaps which map N>1 C args to M>1 S-Lang args
variable array1 = [1,2,4,3];
print("ksink_equals_array_d(array1, array2) returned: %d",
				ksink_equals_array_d(array1, [1,2,3,4]));
print("ksink_equals_array_d(array1, array1) returned: %d",
				ksink_equals_array_d(array1, array1));

print("ksink_copy_string returned: %s",ksink_copy_string("WhoAreYou?"));

ksink_arg_dropper_2();
ksink_arg_dropper_1();
ksink_arg_dropper_1(333222111);

variable s = ksink_params_str( ksink_params_new(1, [3.3, 4.4, 5.5]) );
if (s != "id = 1,   p1=3.300000   p2=4.400000   p3=5.500000")
   print("ksink_params_str failed");
