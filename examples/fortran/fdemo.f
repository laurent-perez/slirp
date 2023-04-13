      PROGRAM quadratic
      
      real a, b, c
      double complex dresult, dcmult
      complex cresult, cmult, fcadd, solution(2)
      common /vars/ out1, out2, out3
      character*8 out1, out2, out3
  
      open(unit=10, file='coeffs.dat')
     
      do while (.TRUE.)
         read (10, *, end=999) a, b, c
         call qsolve(a, b, c, solution)

         call lzpad(3, a, b, c)
         write(*, 100) out1, out2, out3

         call lzpad(2, REAL(solution(1)), AIMAG(solution(1)), 0.0)
         write(*, 200) 'Root #1     ', out1, out2
         call lzpad(2, REAL(solution(2)), AIMAG(solution(2)), 0.0)
         write(*, 200) 'Root #2     ', out1, out2

         dresult = dcmult(DCMPLX(solution(1)), DCMPLX(solution(2)))
c        convert back to single-prec complex, for lzpad
         cresult = CMPLX(dresult)
         call lzpad(2, REAL(cresult), AIMAG(cresult), 0.0)
         write(*, 200) 'D-Multiplied', out1, out2

         cresult = cmult(solution(1), solution(2))
         call lzpad(2, REAL(cresult), AIMAG(cresult), 0.0)
         write(*, 200) 'C-Multiplied', out1, out2

         cresult = fcadd(solution(1), solution(2))
         call lzpad(2, REAL(cresult), AIMAG(cresult), 0.0)
         write(*, 200) 'C-Added     ', out1, out2
         write(*,*)

      end do
     
  100 format('Coefficients: (',A,', ',A,', ',A,')')
  200 format(A,': (',A,', ',A,' i)')
  999 close(10)

      END

c     lzpad exists to account for the fact that some Fortran compilers
c     do not emit leading zeroes when outputting numbers -1 < N < 1.
c     Because of this, simple formatted I/O cannot be relied upon
c     to generate portable numeric output suitable for diff.
c     lzpad is implemented as a subroutine, instead of as a function 
c     whose retval can be fed directly to write, b/c Fortran disallows
c     nested write calls, even when subordinate writes are to a variable
      SUBROUTINE lzpad(num, v1, v2, v3)
      common /vars/ out1, out2, out3
      character*8 out1, out2, out3
      integer num
      real v1, v2, v3

      write(out1, FMT='(F8.4)') v1
      write(out2, FMT='(F8.4)') v2
      if (out1(3:3) .EQ. ' ') out1(3:3) = '0'
      if (out2(3:3) .EQ. ' ') out2(3:3) = '0'

      if (num .EQ. 3) THEN
         write(out3, FMT='(F8.4)') v3
         if (out3(3:3) .EQ. ' ') out3(3:3) = '0'
      endif

      END
