AC_INIT(manticore,0.1,xxx)

AC_PREREQ(2.52)
AC_CONFIG_AUX_DIR(config)

dnl
dnl include extra macros
dnl
sinclude(config/acx_pthread.m4)
sinclude(config/check_leading_uscore.m4)

dnl
dnl get host and target information
dnl
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

case $host_cpu in
  i386|i486|i586|i686) TARGET_CPU=TARGET_X86; ARCH=x86 ;;
  x86_64) TARGET_CPU=TARGET_X86_64; ARCH=x86_64 ;;
  powerpc) TARGET_CPU=TARGET_PPC; ARCH=ppc ;;
  *) AC_MSG_ERROR([host cpu ${host_cpu} is not supported]) ;;
esac
case $host_os in
  linux|linux-gnu) TARGET_OS=TARGET_LINUX ;;
  darwin*) TARGET_OS=TARGET_DARWIN ;;
  *) AC_MSG_ERROR([host os ${host_os} is not supported]) ;;
esac
AC_SUBST(TARGET_CPU)
AC_SUBST(TARGET_OS)

dnl
dnl check for --disable-64 flag
dnl
if test $ARCH = x86_64; then
  AC_ARG_ENABLE(32,
    AC_HELP_STRING(["--enable-32], [enable 32-bit compatibility mode on x86-64]))
  if test x"$enable_32" = xyes ; then
    TARGET_CPU=TARGET_X86
    ARCH=x86
    CC_MODE_FLAG="-m32"
  fi
fi
AC_SUBST(CC_MODE_FLAG)

dnl
dnl check for standard programs
dnl
AC_GNU_SOURCE
AC_PROG_CC
AC_PROG_CPP
AC_CHECK_PROG(AR, ar, ar)
AC_PROG_RANLIB
AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl
dnl check for pthreads
dnl
ACX_PTHREAD()

dnl
dnl check for gcc builtin atomic operations
dnl
AC_MSG_CHECKING([for builtin atomic operations])
AC_LINK_IFELSE(
   AC_LANG_PROGRAM([], [int foo1; int foo2 = __sync_fetch_and_add(&foo1, 1);]),
   AC_MSG_RESULT(yes)
   AC_DEFINE(
    [HAVE_BUILTIN_ATOMIC_OPS], 1,
    [Define to 1 if gcc compiler provides atomic operations.]),
   AC_MSG_RESULT(no))

dnl
dnl check for various library routines
dnl
AC_CHECK_FUNCS(posix_memalign)
if test x"$ac_cv_func_posix_memalign" != xyes ; then
  AC_CHECK_FUNCS(memalign)
  if test x"$ac_cv_func_posix_memalign" != xyes ; then
    AC_CHECK_FUNCS(valloc)
  fi
fi

dnl check for leading "-" on global names
dnl
CHECK_LEADING_UNDERSCORE

dnl ******************** C compiler properties ********************

AC_C_BIGENDIAN
AC_C_INLINE
AC_C_CHAR_UNSIGNED
AC_C_LONG_DOUBLE

dnl
dnl Figure out the sizes of a few things; we use this information in
dnl moby-config.h to determine the type of MOBY_word_t.  And in the
dnl moby-lib/c-interface.mbx file.
dnl
AC_CHECK_SIZEOF(char *)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(unsigned int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
if test x"$ac_cv_c_long_double" = xyes ; then
  AC_CHECK_SIZEOF(long double)
else
  AC_DEFINE(SIZEOF_LONG_DOUBLE, 8, [The number of bytes in a long double])
fi

dnl
dnl config.h stuff
AH_TOP([
/*
 * COPYRIGHT (c) 2006 Manticore project. (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _CONFIG_H_
#define _CONFIG_H_
])
AH_BOTTOM([

#endif /* !_CONFIG_H_ */
])

dnl ******************** Output ********************

AC_CONFIG_FILES(\
dnl ***** Makefiles *****
)
dnl
dnl generate run-time config files
dnl

dnl
dnl create machine-specific links
dnl

AC_OUTPUT

