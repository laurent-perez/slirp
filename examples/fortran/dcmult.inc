      double COMPLEX function dcmult(a, b)
c     Multiply two double-precision complex numbers
      double complex a, b
      dcmult = a * b
      if (dcmult .EQ. 0.0) dcmult = +0.0
      end
