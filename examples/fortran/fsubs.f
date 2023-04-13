      REAL function rzero(r)
c     Avoid output formatting issues between IEEE positive/negative zero
      real r
      if (r .EQ. 0.0) r = +0.0
      rzero = r
      end

      subroutine qsolve(a, b, c, solution)
c     Solve quadratic equation Ax^2 + Bx + C = 0, returning 2 complex roots
      real a, b, c
c      integer*4 z
      complex solution (                2                          )
      real discrm, v, w

      entry altqsolve(a, b, c, solution)

      v = rzero( -b / (2*a) )
      discrm = b*b - 4*a*c

      if ( discrm .LT. 0 ) then
        w = rzero( sqrt(-discrm) / (2*a))
        solution(1) = CMPLX(v, w)
        solution(2) = CMPLX(v, -w)
      else
        w = rzero(sqrt(discrm) / (2*a))
        solution(1) = CMPLX(v+w)
        solution(2) = CMPLX(v-w)
      end if

      end

      include 'dcmult.inc'

      complex function cmult(a, b)
c     Multiply two single-precision complex numbers
      complex a, b
      entry altcmult(a, b)
      cmult = a * b
      if (cmult .EQ. 0.0) cmult = +0.0
      end

      complex function fcadd(a, b)
c     Add two single-precision complex numbers
      complex a, b
      fcadd = a + b
      if (fcadd .EQ. 0.0) fcadd = +0.0
      end

      subroutine inctest1(idum)
      implicit none
      include './inctest1.inc'
      real rarray(zsize)
      real*8 r_8_val
      integer idum
      common /inct1com/ rarray, r_8_val
      parameter (rparam = 1.1, iparam = 999)
      str = 'silly'
      r_8_val = 888.888
      end

      subroutine inctest2()
      include 'inctest2.inc'
      e = 1.1
      i = 2
      d = 'implicitly_declared_string'
      s = .true.
      z = 1
      l = 9.9
      end

      real function rfirstlast(arr, arrlen)
      integer arrlen
      real arr(arrlen)
      entry altfirstlast(arr, arrlen)
      rfirstlast = arr(1) + arr(arrlen)
      end

      subroutine instrarr(arr, len)
c     This demonstrates use of CHARACTER array for both input & output
      integer len
      character*(*) arr(len)
      do i=1,len
        arr(i) = 'blah'
      enddo
      end

      subroutine bad_outstrarr(arr)
      character*(*) arr(*)
      print*,'SLIRP SHOULD REJECT FOR OUTPUT: NO ARRAY SIZE INFO GIVEN'
      end

      subroutine outstrarr(arr)
c     This demonstrates morphing a CHARACTER array arg into output value
      character*6 arr(6)
      entry altoutstrarr(arr)
      arr(1) = 'strout'
      arr(2) = 'strout'
      arr(3) = 'strout'
      arr(4) = 'strout'
      arr(5) = 'strout'
      arr(6) = 'strout'
      end

      subroutine outstrarr2(arr)
c     This demonstrates morphing a 2D CHARACTER array argument into
c     an output value, as well as the use of parameter constants
      parameter(index1=1, N = 3,
     $      index2=2, M=2)
      parameter (x = 3)
      character*7 arr(N,M)
      arr(index1,1) = 'strout1'
      arr(1,2) = 'strout2'
      arr(2,1) = 'strout3'
      arr(2,index2) = 'strout4'
      arr(3,1) = 'strout5'
      arr(3,2) = 'strout6'
      end

      subroutine sub_multi(a,b,c)
c     this routine acts like a function, and is annotated by the
c     interface file to be wrapped as such (i.e. c is "returned")
      real a, b, c
      c = a * b
      end

c     The remaining content is meant to exercise the parser
      function
     *
     *          rtest
c       random comment interspersed with continuation lines
     * (a)
      implicit none
      real rtest
      integer*2 a
      a = 1
      rtest = 0.0
      end

      double precision function imptest(k, d,h,b, m,t,z)
      implicit character*80 (a-c ,         d)
      implicit integer (e,g-h)
      implicit integer*2 (i,j         -m)
      implicit double precision (n      , o ,q-                 t      )
      imptest = 0.0
      end

      subroutine comtest(arr)
      integer icom1
      real rcom1
      double precision fval, dval
      common /com1/ icom1, rcom1
      common fval, dval
      character*10 arr(icom1)
      arr(1) = 'one'
      arr(2) = 'two'
      end

c     FIXME: a slirprc prototype of
c               void comtest2(int* OUTPUT);
c            fails here b/c it turns the OUTPUT array into a scalar

      subroutine comtest2(arr)
      integer icom1
      real rcom1
      common /com1/ icom1, rcom1
      integer arr(icom1)
      arr(1) = 99
      arr(2) = 999
      end

c     FIXME: what should the slirprc prototype look like which turns the
c            array arg iarr of this function into an OUTPUT value?
      real function comtest3(arr)
      integer icom1, icom2
      double precision rcom2
      character*13 scom2
      common /com1/ icom1, rcom1

c     Having rcom2 in the middle of common block here would be a good
c     example of why SLIRP uses Fortran init funcs to obtain pointers
c     to common block variables, instead of C structures:
c
c         g77 -g -O2 -fPIC -c sfwrap_fsubs.f
c               sfwrap_fsubs.f: In subroutine `sfwrapcom_block2_':
c               sfwrap_fsubs.f:81: warning:
c               common        /com_block2_/ icom2,rcom2,scom2
c                                 ^
c         Initial padding for common block `com_block2_' is 4 bytes at (^)
c         -- consider reordering members, largest-type-size first
c
c     This padding is sometimes not reflected in a corresponding C struct
c
c	 extern struct {
c		int icom2; double rcom2; char scom2[13];
c	 } com_block2___;
c
c     and this can result in the structure containing garbage values.

      common /com_block2_/ rcom2, icom2, scom2
      integer icom3arr(3, 2), arr(icom1)
      common /com3/ icom3arr
      arr(1) = 11
      arr(2) = 111
      comtest3 = 9.9
      end

        real function
     *          continuator
     *          (
     *    arg1,  arg2,
     *                  arg_3       , arg4
     *          )
        real * 4 arg1
        real*8 arg2
        character*80 arg_3
        real *4 arg4
        arg1 = 33.33
        arg2 = 44.44
        continuator = arg4 + ichar(arg_3(1:1))+ ichar(arg_3(3:3))
        arg4 = 11.11
        end

      subroutine mmult(a, b, c, M, N, K)
      implicit none
      integer i, row, col, M, N, K
c     Superfluous array indexing used here to exercise parser
      double precision a(M, N), b(1:N,K)
      double precision c(1:M,1:K)
      do row=1,M
        do col=1,K
           c(row,col) = 0
           do i=1,N
                c(row,col) = c(row,col) + a(row,i) * b(i,col)
           enddo
        enddo
      enddo
      end

      double precision function paramtest()
      parameter(pi=3.14159D0,clite=3.D10)
      parameter(herg=6.626D - 27, hev=4.15D-15)
      parameter(big=99.99D+23, bad=123.11 D   - 21)
      parameter(u g l y   =  3.33 e 12)
      paramtest = ugly
      end

      subroutine initcomm(stringval)
      implicit none
      integer icom1, icom2
      real rcom1
      double precision rcom2, dummy, paramtest
      external paramtest
      character*(*) stringval
      character*13 scom2, strarr(3), str
      common /com1/ icom1, rcom1
      common /com_block2_/ rcom2, icom2, scom2
      integer icom3arr(3, 2)
      common /com3/ icom3arr
      complex comp1, comp2(2)
      double complex comp3, comp4(2)
      common /com4/ comp1, comp2, comp3, comp4
      common /com5/ str, strarr
      icom1 = 2
      rcom1 = 9.9
      icom2 = 3
      rcom2 = 333.333
      scom2 = stringval
      icom3arr(1,1) = 1
      icom3arr(1,2) = 2
      icom3arr(2,1) = 3
      icom3arr(2,2) = 4
      icom3arr(3,1) = 5
      icom3arr(3,2) = 6
      comp1 = (3,2)
      comp2(1) = (4,5)
      comp2(2) = (6,7)
      comp3 = (30, 20)
      comp4(1) = (40, 50)
      comp4(2) = (60, 70)
      str = 'simple'
      strarr(1) = 'one'
      strarr(2) = 'two'
      strarr(3) = 'three'

      dummy = paramtest()
      end

      subroutine arraydimdrop(rarray,nidat1,filename,lun11)

      real rarray(nidat1)
      character filename*256

      end

      subroutine baddimignore(rarray,nelems)

c     Fortran compilers will not reject this routine, even though the
c     dim of rarray is implicitly declared.  SLIRP should warn of this.
      real rarray(nelems)

      end

      subroutine sub_with_func_arg(funcptr,dummy)
      external funcptr
      integer dummy
c     SLIRP will ignore this b/c function arguments are not supported yet
      end

      real function func_with_func_arg(funcptr,dummy)
      external funcptr
      integer dummy
c     SLIRP will ignore this b/c function arguments are not supported yet
      func_with_func_arg = 999.99
      end
