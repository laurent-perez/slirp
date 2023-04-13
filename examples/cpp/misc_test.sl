
import("misc");

variable mine = ["green", "cyan", "aqua", "violet", "gray"];
stringlist_print(5, mine);

% Exercise true multi-dimensional array handling, useful to do
% in C++ because of its stricter-by-default type checking
if ( sum2d ( Double_Type[2,2] + 5) != 20) vmessage("sum2d failed");

variable s = StructClass_new(333.333);

if (typeof(s) != StructClass_Type)
   vmessage("Incorrect typed assigned to class defined with struct keyword");

if (not feqs(333.333, StructClass_value_get(s)))
   vmessage("Invalid construction/value of class defined with struct keyword");

if (not feqs(333.333, sca_get_value(s)))
   vmessage("Unable to use StructClass in place of StructClassAlias");

variable c = ClassClass_new(777.777);
if (not feqs(777.777, cca_get_value(c)))
   vmessage("Unable to use ClasClass in place of ClassClassAlias");

!if (is_defined("misc_x") and is_defined("misc_y") and is_defined("misc_z"))
   vmessage("Incorrect parsing of multiple variables declared on single line");

set_farr(3, 99);
if (farr[3] != 99)
   vmessage("Incorrect wrapper handling of intrinsic 1D array variable");

if (strcmp(string_var, "In the year 1492, Columbus sailed the ocean blue"))
   vmessage("String-valued intrinsic variable was not wrapped correctly");

variable s = string_var;
s = 1;				% redefine, to verify no memory issues
