
import("par");

_auto_declare = 1;

private define echo_eval(cmd)
{
   variable t = _stkdepth;
   vmessage(cmd);
   eval("_auto_declare=1; " + cmd);
   if (_stkdepth > t) {
	t = string();
	vmessage(t);
   }
}

echo_eval("x = [PI*100000: PI*200000: .025]");
echo_eval("tic; () = log(x); toc");
echo_eval("tic; () = plog(x); toc");
echo_eval("length( where(log(x) != plog(x)))");

echo_eval("tic; () = sqrt(x); toc");
echo_eval("tic; () = psqrt(x); toc");

echo_eval("tic; () = cos(x); toc");
echo_eval("tic; () = pcos(x); toc");

echo_eval("tic; () = sin(x); toc");
echo_eval("tic; () = psin(x); toc");

echo_eval("tic; () = cos(x) + sin(x); toc");
echo_eval("tic; () = pcos(x) + psin(x); toc");

echo_eval("tic; () = exp(x); toc");
echo_eval("tic; () = pexp(x); toc");
