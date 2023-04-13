      subroutine fadd(x, y, len, result)
      integer i, len
c     Superfluous array index notation used here to exercise parser
      double precision x(1:len), y(1:len), result(1:len)

      do i=1,len
           result(i) = x(i) + y(i)
      enddo

      end

      function fsin(x)
      double precision fsin, x
           fsin = sin(x)
      end

      complex function fcadd(a, b)
c     Add two single-precision complex numbers
      complex a, b
      fcadd = a + b
      if (fcadd .EQ. 0.0) fcadd = +0.0
      end

      subroutine fmprint(matrix, nrows, ncols)
      integer r, c, nrows, ncols
      double precision matrix(nrows,ncols)
      do r=1,nrows
        do c=1,ncols
            write(*,10) r, c, matrix(r,c)
        enddo
      enddo
   10 FORMAT('FORTRAN matrix(',I1,',',I1,') = ',F5.2)
      end

      subroutine faprint(array, nelems)
      integer i, nelems
      double precision array(nelems)
      do i=1,nelems
            write(*,10) i, array(i)
      enddo
   10 FORMAT('FORTRAN array(',I1,') = ',F5.2)
      end
