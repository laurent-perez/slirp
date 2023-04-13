-*- mode: m4 -*-

AC_PREREQ(2.59)
AC_INIT(slirp, dummy)
AC_PREFIX_DEFAULT("/usr/local"
if test -f "/usr/include/slang.h"; then
   ac_default_prefix="/usr"
fi
)

echo ""
echo "Configuring SLIRP, the S-Lang interface package ..."
echo ""

AC_CONFIG_AUX_DIR(admin)
AC_CONFIG_SRCDIR([slirp.sl])
AC_CANONICAL_HOST
AC_PROG_INSTALL
AC_PROG_LN_S
JD_INIT
JD_ANSI_CC
GCC_WARNINGS
MN_GET_SO_SETTINGS
SAFE_AC_PROG_FC
SAFE_AC_FC_MANGLE
AC_FIND_SLANG(2.1.0)
AC_FIND_SLSH
AC_GET_DATE
AC_SLIRP_FINALIZE

AC_CONFIG_HEADERS([config.h:config.h.in])

AC_CONFIG_FILES([
Makefile
unix.mf
slirputils.sl
builtins.sl
intro.h
slirp.ins
examples/Makefile.inc
])

if test -f www/index.html.in  ; then
AC_CONFIG_FILES([www/index.html])
(cd www ; ! test -e && ln -s ../doc )
fi

if test -f doc/tm/manual.tm.in  ; then
AC_CONFIG_FILES([doc/tm/manual.tm])
fi

AC_OUTPUT

# Generate slirp script usable directly from devel tree, as opposed
# to slirp.ins, which is intended to be used from the install tree
cat slirp.ins | sed -e "/^prefix=.*$/d" \
		    -e "s%^BinDir=.*$%BinDir=$CONFIG_DIR%" \
		    -e "s%^ScriptsDir=.*$%ScriptsDir=$CONFIG_DIR%" > slirp

chmod +x slirp safe_slsh

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
echo "  FCFLAGS            : $FCFLAGS"
echo "  OPENMP_CFLAGS      : $OPENMP_CFLAGS"
echo ""
echo "You may need to edit the generated Makefile.  Type 'make' to build "
echo "SLIRP, and optionally 'make install'"
