import("vec");

% Simple demo of C --> FORTRAN array transposition.  C arrays > 1D will
% be automatically tranposed to/from a FORTRAN memory layout; however,
% FORTRAN funcs with array args >1D will NOT be vectorized.

define cmprint(m)
{
   variable r,c ;
   for (r=0; r<3; r++)
      for (c=0; c<2; c++)
	 () = printf("C matrix(%d,%d) = %4.2f\n",r+1,c+1,m[r,c]);
}

public variable m1 = _reshape([1,2,3,4,5,6], [3,2]);

vmessage("Each sequence of numbers output below should be identical.\n");

% First, an ordinary C 2D matrix print
cmprint(m1);
% Now, a FORTRAN 2D matrix print (showing automatic transposition)
fmprint(m1,3,2);
% Now, a vectorized FORTRAN 1D array print (showing tranposition is avoided)
faprint(m1);
% Finally, ordinary C 2D matrix print again (showing original array unharmed)
cmprint(m1);
