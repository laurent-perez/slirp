dnl# -*- sh -*-
AC_INIT(src/preproc-module.c)
AC_PREFIX_DEFAULT(/usr/local)
AC_CONFIG_AUX_DIR(autoconf)

echo ""
echo "Configuring SLIRP, the S-Lang interface package ..."
echo ""

AC_CONFIG_AUX_DIR(autoconf)
AC_CONFIG_SRCDIR([src/slirp.sl])
AC_CANONICAL_HOST
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

JD_INIT
JD_ANSI_CC
JD_GCC_WARNINGS

m4_include([./include/slirp.m4])


MN_GET_SO_SETTINGS
SAFE_AC_PROG_FC
SAFE_AC_FC_MANGLE
JD_TERMCAP
AC_FIND_SLANG(2,1,0)
AC_GET_DATE
AC_SLIRP_FINALIZE

AC_CONFIG_HEADER(src/config.h:src/config.hin)
AC_OUTPUT(Makefile:autoconf/Makefile.in src/Makefile)
AC_OUTPUT(src/slirpconf.sl:src/slirpconf.sl.in)
AC_OUTPUT(src/lib/unix.mf:src/lib/unix.mf.in)
AC_OUTPUT(src/lib/intro.h:src/lib/intro.h.in)
AC_OUTPUT(examples/Makefile.inc:examples/Makefile.inc.in)

if test -f www/index.html.in  ; then
AC_CONFIG_FILES([www/index.html])
(cd www ; ! test -e && ln -s ../doc )
fi

if test -f doc/tm/manual.tm.in  ; then
AC_CONFIG_FILES([doc/tm/manual.tm])
fi

AC_OUTPUT

echo ""
echo "Configuration complete.  You are using S-Lang version ${SLANG_VERSION}."
echo ""
echo "  prefix             : $prefix"
echo "  exec_prefix        : $exec_prefix"
echo "  Package Install Dir: $SL_FILES_INSTALL_DIR"
echo ""
echo "  SLANG_INC          : $SLANG_INC"
echo "  SLANG_LIB          : $SLANG_LIB"

if test "$CC" = "gcc" ; then
   c_compiler="`gcc --version | head -1`"
else
   c_compiler="$CC"
fi

echo "  C compiler         : $c_compiler"
echo "  C++ compiler       : $CXX"
echo "  Fortan compiler    : $FC"
echo ""
echo "  CFLAGS             : $CFLAGS"
echo "  SHARED OBJECT FLAGS: $SO_CFLAGS"
echo "  LDFLAGS            : $LDFLAGS"
echo "  FCFLAGS            : $FCFLAGS"
echo "  OPENMP_CFLAGS      : $OPENMP_CFLAGS"
if test "$Unsupported_OS" = 1; then
    echo "**************************************************************"
    echo "NOTE: This system (host_os=$host_os) is not fully supported."
    echo "      The C, C++, and Fortan compilers and flags may be wrong."
    echo "      Be sure to run \`make check\` before installation."
    echo "**************************************************************"
fi
echo ""
echo "You may need to edit the generated Makefile.  Type 'make' to build "
echo "SLIRP, 'make check' to test it, and optionally 'make install'"
