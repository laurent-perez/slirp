c     Older versions of gfortran have broken implementations of
c     of list-directed reads with commas, so we check for that.

      program iobug

      character(len=80) buff
      logical broken

      a = 0
      b = 0
      c = 0
      d = 0
      write (buff,'(a)') '10,20,30,40'
      read(buff,*) a, b, c, d

      broken = (abs(10. - a) > 1e-5) .OR. (abs(20. - b) > 1e-5)
      broken = broken .OR. (abs(30. - c) > 1e-5)
      broken = broken .OR. (abs(40. - d) > 1e-5)

      if (broken) then
        print*,'Your FORTRAN compiler is broken.  It does not properly'
        print*,'support list-directed reads with embedded commas.  Try'
        print*,'re-configuring SLIRP using FC=<other_fortran_compiler>.'
      end if

      end program

