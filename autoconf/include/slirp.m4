dnl# -*- mode: sh; mode: fold -*-

AC_DEFUN(MN_GET_SO_SETTINGS,  dnl#{{{
[
dnl#---------------------------------------------------------------------
dnl# Check for shared object/dynamic linker
dnl#---------------------------------------------------------------------
DYNAMIC_LINK_LIB=""
AC_CHECK_HEADER(dlfcn.h,[
  AC_DEFINE(HAVE_DLFCN_H)
  AC_CHECK_LIB(dl,dlopen,[
    DYNAMIC_LINK_LIB="-ldl"
    AC_DEFINE(HAVE_DLOPEN)
   ],[
    AC_CHECK_FUNC(dlopen,AC_DEFINE(HAVE_DLOPEN))
    if test "$ac_cv_func_dlopen" != yes
    then
      AC_MSG_WARN(cannot perform dynamic linking)
    fi
   ])])
AC_SUBST(DYNAMIC_LINK_LIB)

SO="\$(MODULE)"
SO_MAJOR_VER=1
SO_MINOR_VER=0.0
SO_MAJOR_NAME="\$(SO).\$(SO_MAJOR_VER)"

dnl# ---------------------------------------------------------------------
dnl# Add path to CC, for robust makes on systems with many compilers
dnl# ---------------------------------------------------------------------

dnl# CC_PATH=`AS_DIRNAME([$CC])`
dnl# AX_PATH_PROG([CC],`basename $CC`, $CC, $CC_PATH)

STRIP="strip"		dnl# default to standard UNIX / X11 settings
SH_EXT=.so
PATH_SEP=$PATH_SEPARATOR

AC_PROG_CXX

PIC_FLAG=-fPIC		# default to linux/gcc setting for dynamic code
SO_CFLAGS="\$(CFLAGS)"
C_LINK="\$(CC) \$(SO_LINK_FLAGS)"
CXX_LINK="\$(CXX) \$(SO_LINK_FLAGS)"
DYNAMIC_LINK_FLAGS=
INSTALL_MODULE="\$(INSTALL_DATA)"

Unsupported_OS=0
case "$host_os" in

  *linux* | *gnu*)		dnl#{{{

    CC_SHARED="\$(CC) \$(CFLAGS) -shared"
    SO_LINK_FLAGS="-shared -Wl,-soname,\$(SO_MAJOR_NAME)"

    if test "$GCC" = yes ; then
	DYNAMIC_LINK_FLAGS="-Wl,-export-dynamic"
	SO_DEP_LIBS="\$(DL_LIB) -lm -lc"
    else
	SO_DEP_LIBS="\$(DL_LIB)"
    fi
    ;;	dnl#}}}

  *solaris* )	dnl#{{{

    CC_SHARED="\$(CC) \$(CFLAGS) -G"
    SO_DEP_LIBS="\$(DL_LIB) -lm -lc"

    if test "$GCC" = yes ; then
      SO_LINK_FLAGS="-shared -Wl,-ztext -Wl,-h,\$(SO_MAJOR_NAME)"
    else
      PIC_FLAG="-KPIC"
      SO_LINK_FLAGS="-G -h\$(SO_MAJOR_NAME)"

      if test `basename $CXX` != "CC"; then
	AC_MSG_ERROR(only Solaris CC compiler supported in this context)
      fi

      CXX_VERSION=`$CXX -V 2>&1| sed "s/.*\([[1-9]][[1-9]]*\.[[1-9]][[1-9\]]*\).*/\1/"`
      case $CXX_VERSION in
       3*) AC_MSG_ERROR(compiler is too old);;
       4*) CXX_LIBS="-lC";;
       *)  CXX_LIBS="-lCstd -lCrun";;
      esac

    fi
    ;;	dnl#}}}

  *sco3.2v5* | *unixware-5* | *sco-sysv5uw7*)   dnl#{{{

     # osr5 or unixware7 with current or late autoconf

     SO_DEP_LIBS=
     CC_SHARED="\$(CC) \$(CFLAGS) -G"

     if test "$GCC" = yes ; then 
       SO_LINK_FLAGS="-shared -Wl,-h,\$(SO_MAJOR_NAME)"
     else
       PIC_FLAG="-K pic"
       SO_LINK_FLAGS="-G -z text -h\$(SO_MAJOR_NAME)"
     fi
     ;;  dnl#}}}

  *irix6.5* )   dnl#{{{

     echo "Note: Shared-object compiler for host_os=$host_os may not be correct"
     echo "double-check: 'mode_t', 'pid_t' may be wrong!"

     CC_SHARED="\$(CC) \$(CFLAGS) -shared"
     SO_DEP_LIBS=

     if test "$GCC" = yes ; then 
       # not tested
       SO_LINK_FLAGS="-shared -Wl,-h,\$(SO_MAJOR_NAME)"
     else
       PIC_FLAG="-K pic"
       SO_LINK_FLAGS="-shared -o \$(SO_MAJOR_NAME)"
     fi
     ;;  dnl#}}}

  *darwin* )   dnl#{{{

     PIC_FLAG=
     STRIP=":"
     DYNAMIC_LINK_FLAGS=""
     SO_CFLAGS="$SO_CFLAGS -fno-common"
     dnl# Darwin linker doesn't like -compatibility_version of 0, so we omit it
     SO_LINK_FLAGS="-dynamiclib -single_module -install_name \$(MODULE_INSTALL_DIR)/\$(SO_MAJOR_NAME) -current_version \$(SO_MAJOR_VER).\$(SO_MINOR_VER)"

     SO_DEP_LIBS="$LDFLAGS \$(DL_LIB)"
     CC_SHARED="\$(CC) -bundle -flat_namespace -undefined suppress \$(CFLAGS) -fno-common"
     ;;  dnl#}}}

  *cygwin* )   dnl#{{{
    SH_EXT=.dll.a
    PIC_FLAG=
    SO_LINK_FLAGS="-shared"
    SO_DEP_LIBS="\$(DL_LIB) -lm"
    CC_SHARED="\$(CC) \$(CFLAGS) -shared"
    INSTALL_MODULE="\$(INSTALL)"
    ;;  dnl#}}}

  *mingw* )   dnl#{{{

    PIC_FLAG=
    SH_EXT=.dll
    SO_LINK_FLAGS="-shared"
    SO_DEP_LIBS="\$(DL_LIB) -lm"
    CC_SHARED="\$(CC) \$(CFLAGS) -shared"
    dnl# avoid Unix/Win CR/LF issues
    COLLAPSE_BLANKS_FOR_REGR_TESTS="-b"
    CONFIG_DIR=`pwd -W`
    PATH_SEP=';'
    ;;  dnl#}}}

  * )   dnl#{{{

    STRIP=":"
    Unsupported_OS=1
    SO_LINK_FLAGS="-shared"
    SO_DEP_LIBS="\$(DL_LIB) -lm -lc"
    CC_SHARED="\$(CC) \$(CFLAGS) -shared"
    CXX_LIBS="-lstdc++"
    ;;  dnl#}}}

esac

SO_CFLAGS="$SO_CFLAGS $PIC_FLAG"
CC_SHARED="$CC_SHARED $PIC_FLAG"

AC_SUBST(INSTALL_MODULE)
AC_SUBST(PIC_FLAG)
AC_SUBST(C_LINK)
AC_SUBST(CXX_LINK)
AC_SUBST(CXX_LIBS)
AC_SUBST(DYNAMIC_LINK_FLAGS)
AC_SUBST(CC_SHARED)
AC_SUBST(SO)
AC_SUBST(SO_MAJOR_NAME)
AC_SUBST(SO_MAJOR_VER)
AC_SUBST(SO_MINOR_VER)
AC_SUBST(SO_CFLAGS)
AC_SUBST(SO_LINK_FLAGS)
AC_SUBST(SO_DEP_LIBS)
AC_SUBST(STRIP)
AC_SUBST(SH_EXT)
AC_SUBST(COLLAPSE_BLANKS_FOR_REGR_TESTS)
AC_SUBST(PATH_SEP)
]) dnl#}}}

AC_DEFUN(SAFE_AC_PROG_FC, dnl#{{{
[
   AC_REQUIRE([MN_GET_SO_SETTINGS]) dnl# mainly for PIC_FLAG
   dnl# The AC_PROG_FC macro in autoconf 2.59 is broken, in that it aborts when
   dnl# a FORTRAN compiler cannot be found, instead of simply setting FC=""
   pushdef([AC_MSG_ERROR], [HAVE_FC=0])
   AC_PROG_FC
   if test -z "$FC" || test "X$FC" = "Xno"; then
	FC=
	HAVE_FC=0
   else
	HAVE_FC=1
	FCFLAGS="$FCFLAGS $PIC_FLAG"
   fi
   popdef([AC_MSG_ERROR])
   AC_SUBST(HAVE_FC)
])dnl#}}}

AC_DEFUN(SAFE_AC_FC_LIBRARY_LDFLAGS, dnl#{{{
[
   AC_REQUIRE([SAFE_AC_PROG_FC])
   AC_REQUIRE([AC_FC_LIBRARY_LDFLAGS])
   dnl# Solaris OpenMP stubs library must appear last on link line
   dnl# when linking mixed C/FORTRAN codes with C compiler, but
   dnl# the linker -v flag doesn't do this; so we fix it here
   _AC_LIST_MEMBER_IF(-lompstubs, $FCLIBS, [
	FCLIBS=`echo $FCLIBS | sed 's/-lompstubs//g'`
	FCLIBS="$FCLIBS -lompstubs" ]
	)
])dnl#}}}

AC_DEFUN(SAFE_AC_FC_MANGLE, dnl#{{{
[
   AC_REQUIRE([SAFE_AC_FC_LIBRARY_LDFLAGS])
   pushdef([AC_MSG_ERROR], [FC_MANGLE_UPCASE=0; FC_MANGLE_SUFFIX= ; FC_MANGLE_EXTRA_USCORE=0; FCRPATH= ])
   if test $HAVE_FC -eq 1 ; then
	AC_FC_WRAPPERS
	AC_FC_FUNC(a)
	case $a in
	   A)	FC_MANGLE_UPCASE=1; FC_MANGLE_SUFFIX="";;
	   A_)	FC_MANGLE_UPCASE=1; FC_MANGLE_SUFFIX="_";;
	   A__)	FC_MANGLE_UPCASE=1; FC_MANGLE_SUFFIX="__";;
	   a)	FC_MANGLE_UPCASE=0; FC_MANGLE_SUFFIX="";;
	   a_)	FC_MANGLE_UPCASE=0; FC_MANGLE_SUFFIX="_";;
	   a__)	FC_MANGLE_UPCASE=0; FC_MANGLE_SUFFIX="__";;
	   *)	AC_MSG_WARN([unknown Fortran mangling convention])
		FC_MANGLE_UPCASE=0; FC_MANGLE_SUFFIX="_"
		AC_MSG_WARN([A=$a])
		;;
	esac

	AC_FC_FUNC(_,a)
	case $a in
	_${FC_MANGLE_SUFFIX})   FC_MANGLE_EXTRA_USCORE=0;;
	_${FC_MANGLE_SUFFIX}_)  FC_MANGLE_EXTRA_USCORE=1;;
	*)   AC_MSG_WARN([unknown Fortran extra-underscore mangling convention])
	     FC_MANGLE_EXTRA_USCORE=0;;
	esac

	SAVE_RPATH=$RPATH
	RPATH=""
	for l in $FCLIBS; do
	   opt=`echo $l | cut -c1-2`
	   if test "$opt" = "-L" ; then
		JD_SET_RPATH(`echo $l | cut -c3-`)
	   else
		FCLIBS_NAMES="$FCLIBS_NAMES $l"
	   fi
	done
	FCRPATH=$RPATH
	RPATH=$SAVE_RPATH
   fi
   popdef([AC_MSG_ERROR])
   AC_SUBST(FC_MANGLE_UPCASE)
   AC_SUBST(FC_MANGLE_SUFFIX)
   AC_SUBST(FC_MANGLE_EXTRA_USCORE)
   AC_SUBST(FCRPATH)
   AC_SUBST(FCLIBS_NAMES)
])dnl#}}}

AC_DEFUN(AX_PATH_SET, dnl#{{{
[
   AX_PATH_PROG_PATH=$PATH:/bin:/usr/bin:/usr/local/bin:/opt/local/bin:/sw/bin
])dnl#}}}

AC_DEFUN(AX_PATH_PROG, dnl#{{{
[
   AC_REQUIRE([AX_PATH_SET])
   AC_PATH_PROG([$1],[$2],[$3],[$AX_PATH_PROG_PATH:$4])
])dnl#}}}

AC_DEFUN(AC_GET_DATE, dnl#{{{
[
  AX_PATH_PROG([DATE],[date],[])
  if test -n "$DATE" ; then
	date=`${DATE}`
  else
	date=unknown
  fi
  AC_SUBST(date)
])dnl#}}}

AC_DEFUN(GET_TYPE_MNEMONIC_I, dnl#{{{
[
  case $2 in
	1)  $1[]_mnemonic=$3char;;
	$ac_cv_sizeof_short) $1[]_mnemonic=$3short;;
	$ac_cv_sizeof_int) $1[]_mnemonic=$3int ;;
	$ac_cv_sizeof_long) $1[]_mnemonic=$3long ;;
	$ac_cv_sizeof_long_long) $1[]_mnemonic=$3llong;;
	*) AC_MSG_ERROR([could not determine type mnemonic for $1]);;
  esac
  AC_SUBST($1[]_mnemonic)
])
dnl#}}}

AC_DEFUN(GET_TYPE_MNEMONIC_R, dnl#{{{
[
  case $2 in
	$ac_cv_sizeof_float) $1[]_mnemonic=float ;;
	$ac_cv_sizeof_double) $1[]_mnemonic=double;;
	$ac_cv_sizeof_long_double) $1[]_mnemonic=ldouble;;
	*)  $1[]_mnemonic=unsupported;;
  esac
  AC_SUBST($1[]_mnemonic)
])
dnl#}}}

AC_DEFUN(AC_SLIRP_FINALIZE, dnl#{{{
[
   if ! `echo $ac_configure_args | grep '\-\-datadir' >/dev/null` ; then
	datadir='${datarootdir}/slirp'
   fi

   AC_SUBST(nvec)
   AC_ARG_WITH(nvec,
      [  --with-nvec=NUM		Set maximum number of function arguments
				which may be vectorized (default==10)],
	nvec=$withval, nvec=10)

   JD_UPPERCASE(AC_PACKAGE_NAME,MODULE_NAME)
   SLIRP_URL=http://space.mit.edu/cxc/slirp/
   AC_SUBST(SLIRP_URL)

   AC_CHECK_HEADER(stdint.h, HAVE_STDINT_H=1, HAVE_STDINT_H=0)
   AC_SUBST(HAVE_STDINT_H)

   AC_CHECK_SIZEOF(short)
   AC_CHECK_SIZEOF(int)
   AC_CHECK_SIZEOF(long)
   AC_CHECK_SIZEOF(long long)
   AC_CHECK_SIZEOF(unsigned long long)
   AC_CHECK_SIZEOF(float)
   AC_CHECK_SIZEOF(double)
   AC_CHECK_SIZEOF(long double)
   AC_CHECK_SIZEOF(char *)
   AC_CHECK_SIZEOF(ptrdiff_t)
   AC_CHECK_SIZEOF(size_t)

   AX_OPENMP
   AC_SUBST(OPENMP_CFLAGS)	dnl# assume same flags used for C++
   if test -n "$OPENMP_CFLAGS" ; then
	AC_MSG_CHECKING(that supported OpenMP specification is 2.0 or later)
	_save_cflags=$CFLAGS
	CFLAGS="$OPENMP_CFLAGS $CFLAGS"
	AC_TRY_RUN([int main() { if (_OPENMP >= 200000) return 0; return 1; }],
	   AC_MSG_RESULT(yes),
	   [AC_MSG_RESULT([no, OpenMP will not be used]) ; OPENMP_CFLAGS= ])
	CFLAGS=$_save_cflags
   fi

   GET_TYPE_MNEMONIC_I(ptrdiff_t, $ac_cv_sizeof_ptrdiff_t)
   GET_TYPE_MNEMONIC_I(size_t, $ac_cv_sizeof_size_t, u)
   GET_TYPE_MNEMONIC_I(int16, 2)
   GET_TYPE_MNEMONIC_I(uint16, 2, u)
   GET_TYPE_MNEMONIC_I(int32, 4)
   GET_TYPE_MNEMONIC_I(uint32, 4, u)
   GET_TYPE_MNEMONIC_I(int64, 8)
   GET_TYPE_MNEMONIC_I(uint64, 8, u)
   GET_TYPE_MNEMONIC_I(long_long, $ac_cv_sizeof_long_long)
   GET_TYPE_MNEMONIC_I(ulong_long, $ac_cv_sizeof_long_long, u)

   AC_LANG_PUSH(C++)
   AC_CHECK_SIZEOF(bool)
   AC_LANG_POP()

   if test $ac_cv_sizeof_bool -eq 0 ; then
	ac_cv_sizeof_bool=1
   fi
   GET_TYPE_MNEMONIC_I(bool, $ac_cv_sizeof_bool)
   GET_TYPE_MNEMONIC_R(float32, 4)
   GET_TYPE_MNEMONIC_R(float64, 8)
   GET_TYPE_MNEMONIC_R(float96, 12)
   test $ac_cv_sizeof_long_long -eq 0
   HAVE_LONG_LONG=$?
   AC_SUBST(HAVE_LONG_LONG)

   AX_PATH_PROG(SET_SECURITY,chcon,touch)dnl# check for SELinux security
   if test "$SET_SECURITY" != "touch" ; then
	SET_SECURITY="$SET_SECURITY -t texrel_shlib_t"
   fi
   AC_SUBST(SET_SECURITY)

   dnl# Use full path to compilers, to avoid crosstalk in generated Makefiles
   dnl# on systems with multiple compilers (can be overridden at 'make' time)
   COMPILER_PATH=`AS_DIRNAME([$CC])`
   AX_PATH_PROG([CC],`basename $CC`, $CC, $COMPILER_PATH)
   COMPILER_PATH=`AS_DIRNAME([$CXX])`
   AX_PATH_PROG([CXX],`basename $CXX`, $CXX, $COMPILER_PATH)
   COMPILER_PATH=`AS_DIRNAME([$FC])`
   AX_PATH_PROG([FC],`basename $FC`, $FC, $COMPILER_PATH)

   # Solaris requires -xO3 optimization for OpenMP & that apps into which
   # OpenMP-aware dynamic libs will be loaded ALSO be linked with OpenMP
   if test "$OPENMP_CFLAGS" = "-xopenmp" ; then
	LDFLAGS="$OPENMP_CFLAGS $LDFLAGS"
	CFLAGS="$CFLAGS -xO3"
   fi
])
dnl#}}}

AC_DEFUN([AX_OPENMP], [ 	dnl#{{{
dnl# OpenMP macro Copyright 2006 Steven G. Johnson (stevenj@alum.mit.edu)
AC_PREREQ(2.59) dnl# for _AC_LANG_PREFIX

AC_CACHE_CHECK([for OpenMP flag of _AC_LANG compiler], ax_cv_[]_AC_LANG_ABBREV[]_openmp, [save[]_AC_LANG_PREFIX[]FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
ax_cv_[]_AC_LANG_ABBREV[]_openmp=unknown
# Flags to try:  -fopenmp (gcc), -openmp (icc), -mp (SGI &amp; PGI),
#                -xopenmp (Sun), -omp (Tru64), -qsmp=omp (AIX), none
ax_openmp_flags="-fopenmp -openmp -mp -xopenmp -omp -qsmp=omp none"
if test "x$OPENMP_[]_AC_LANG_PREFIX[]FLAGS" != x; then
  ax_openmp_flags="$OPENMP_[]_AC_LANG_PREFIX[]FLAGS $ax_openmp_flags"
fi
for ax_openmp_flag in $ax_openmp_flags; do
  case $ax_openmp_flag in
    none) []_AC_LANG_PREFIX[]FLAGS=$save[]_AC_LANG_PREFIX[] ;;
    *) []_AC_LANG_PREFIX[]FLAGS="$save[]_AC_LANG_PREFIX[]FLAGS $ax_openmp_flag" ;;
  esac
  AC_TRY_LINK_FUNC(omp_set_num_threads,
        [ax_cv_[]_AC_LANG_ABBREV[]_openmp=$ax_openmp_flag; break])
done
[]_AC_LANG_PREFIX[]FLAGS=$save[]_AC_LANG_PREFIX[]FLAGS
])
if test "x$ax_cv_[]_AC_LANG_ABBREV[]_openmp" = "xunknown"; then
  m4_default([$2],:)
else
  if test "x$ax_cv_[]_AC_LANG_ABBREV[]_openmp" != "xnone"; then
    OPENMP_[]_AC_LANG_PREFIX[]FLAGS=$ax_cv_[]_AC_LANG_ABBREV[]_openmp
  fi
  m4_default([$1], [AC_DEFINE(HAVE_OPENMP,1,[Define if OpenMP is enabled])])
fi
])dnl# AX_OPENMP #}}}

AC_DEFUN(AC_FIND_SLANG, dnl#{{{
[
JD_WITH_LIBRARY(slang)

JD_SLANG_VERSION

echo "SLang version is $slang_version"
required_slang_version_as_int=`echo $1 | tr '.' '0'`
if test -z "$slang_version" || test $slang_version -lt $required_slang_version_as_int ; then
   AC_MSG_ERROR([S-Lang $1 or later is required; use --with-slang option to specify an alternate installation of S-Lang])
fi
SLANG_VERSION=$slang_version
SLANG_LIB_STATIC=`echo $SLANG_LIB| cut -c 3-`/libslang.a
AC_SUBST(SLANG_LIB_STATIC)
AC_SUBST(SLANG_VERSION)

JD_SLANG_MODULE_INSTALL_DIR
])dnl#}}}


