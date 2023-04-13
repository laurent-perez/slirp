% This is superset of the triv.sl example, which adds in failure tests

import("cppdemo");
() = evalfile("./cppdemo.sl");

dump_stack("Demo Script");
print("*** Test: demo done, now executing more stringent cases ***\n\n");

check_ignored("Computation_new");
check_ignored("Computation_silly");

static variable pi = Pi_new(500);
validate_state(pi, Computable);

_try(&Computation_compute, pi);
_try(&Pi_compute, pi);

pi = Pi_new(100);
Computation_set_verbose(pi, 0);
variable result = 1000*Pi_compute(pi);
variable arr = Double_Type[10] + 1;
Computation_mult(pi, arr);
if ( int(100*sum(arr)) != int(result) )
   print("Computation_mult(Pi) failed ...");

% Test get/set methods for public fields
if (Computation__state_get(pi) != Computed)
   print("Pi should be in 'Computed' state, but is not");
Computation__state_set(pi, Computable);
if (Computation__state_get(pi) != Computable)
   print("Pi should NOW be in 'Computable' state, but is not");

variable eh = ErrorHandler_new();	% Take over SLang_Error_Hook ...

pi = NULL;				% ... then create intentional failures
_try(&Computation_compute, pi);
_try(&Pi_compute, pi);
_try(&Pi_new, "dummy");			% generate overloaded Usage: messages
_try(&output, "s");
_try(&output, &pi);
_try(&output);
_try(&output, 3, [999.0, 888, 777], [555.0, 444, 333]);
print("");
_try(&output, 3, [222.0, 111, 222]);

% Verify that the SLIRP dispatcher lets arrays of dissimilar types
% through (S-Lang will flag illegal casts when at pop time)
print("");
_try(&output, typecast([44, 55, 66, 77], Short_Type));
_try(&output, ["one","two"]);

% Observe that we do not instanstiate a ProcessGlobal object here, but 
% rather use the instance declared in compiled scope (see interface file)
ProcessGlobal_report();

variable v1 = Vec_new(3,4), v2 = v1, dp = Vec_dot(v1, v2);
if (dp != 25)
   print("v1 dot v2 == %f, not 25!", dp);

!if (istrue(1) ) print("istrue(1) failed!");
if (istrue(0)) print("istrue(0) failed!");
!if (istrue(3025) ) print("istrue(3025) failed!");
