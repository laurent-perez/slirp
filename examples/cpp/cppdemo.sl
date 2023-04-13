import("cppdemo");
() = evalfile("../examples-common");

% Keep numeric output compatibility between S-Lang 2.2 & earlier versions
set_float_format("%3.5f");

variable pi = Pi_new();

print("typeof(Pi instance) = <%S>", _typeof(pi));
invoke_zero_arg_methods(pi);

pi = Pi_new(100);			% This invokes destructor, on previous

variable id_r = Computation_id_r(pi);
if (id_r != Computation_id(pi))
    print("id_r(long&) S-Lang wrapper is broken, it returned <%S>",id_r);


print("Pi_compute(100 intervals) = <%S>\n",Computation_compute(pi));

pi = Pi_new(pi);
print("Pi_new(pi), Pi_compute() = <%S>\n",Computation_compute(pi));
pi = NULL;				% This invokes destructor, on current

output(3.2);
output(322);
output( typecast(322, Short_Type) );
output(123456);

variable darr = [1.1, 2.2, 3.3, 4.4 ];
output(darr, length(darr));
output(darr);				% Exercise default argument handling

output([101, 202, 303, 404, 505], 5);	% Arrays s/b distinguishable by type

variable d = 333.333;
change(d);				% d is passed as a read-only ref here,
output(d);				% so it's value should remain unchanged
