import("par");

private define validate(thing1, thing2, name)
{
   variable diffs = where(thing1 != thing2);
   if (length(diffs)) {
	variable elem = diffs[0];
	verror("%s broken, elem %d differed:\n"+
	       "sequential = %f, parallel = %f", name, elem,
					thing1[elem], thing2[elem]);
   }
}

private define compare(func, x)
{
   variable name = sprintf("%S",func)[[1:]], pname = "p" + name;
   variable pfunc = __get_reference(pname);

   variable f  = (@func)  (x);;
   variable pf = (@pfunc) (x);

   validate(f, pf, pname);
}

_auto_declare = 1;

x = [PI : PI*100: PI/10000];
compare(&sin, x);
compare(&cos, x);
compare(&log, x);
compare(&sqrt, x);

cs = cos(x) + sin(x);
pcs = pcos(x) + psin(x);
validate(cs, pcs, "pcos() + psin()");

nstrings=300000;
s = array_map(String_Type, &sprintf, "string-%d", [1:nstrings]);
sl = array_map(Int_Type, &strlen, s);
psl = pstrlen(s);
validate(sl, psl, "pstrlen");
