dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2007 The Manticore Project (manticore.cs.uchicago.edu)
dnl All rights reserved.
dnl

dnl NOTE: MANTICORE_DIST_VERSION is an M4 variable defined in config/aclocal.m4
dnl
AC_INIT(manticore,MANTICORE_DIST_VERSION,http://manticore.cs.uchicago.edu)

AC_PREREQ(2.59)
AC_CONFIG_AUX_DIR(config)

dnl
dnl include extra macros
dnl
sinclude(config/check_smlnj.m4)
sinclude(config/check_smlnj_heap_suffix.m4)
sinclude(config/acx_pthread.m4)
sinclude(config/check_leading_uscore.m4)

dnl
dnl get host and target information
dnl
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
if test $target_cpu != $host_cpu ; then
  CROSS_COMPILING=1
  RUNTIME_DIR=
else
  CROSS_COMPILING=0
  RUNTIME_DIR=runtime
fi
AC_SUBST(CROSS_COMPILING)

case $target_cpu in
  i386|i486|i586|i686) TARGET_CPU=TARGET_X86; ARCH=x86 ;;
  x86_64) TARGET_CPU=TARGET_X86_64; ARCH=x86_64 ;;
  *) AC_MSG_ERROR([target cpu ${target_cpu} is not supported]) ;;
esac
case $target_os in
  linux|linux-gnu) TARGET_OS=TARGET_LINUX; OS=linux; LDFLAGS="$LDFLAGS -lm" ;;
  darwin*) TARGET_OS=TARGET_DARWIN; OS=darwin ;;
  *) AC_MSG_ERROR([target os ${target_os} is not supported]) ;;
esac

dnl
dnl check to see if a x86-darwin host supports 64-bit executables
dnl
if test $OS = darwin -a $ARCH = x86 -a $CROSS_COMPILING = 0; then
  if test -x /usr/sbin/sysctl ; then
    AC_MSG_CHECKING([for 64-bit support])
    /usr/sbin/sysctl -n machdep.cpu.extfeatures | grep -q EM64T
    if test $? = 0 ; then
      TARGET_CPU=TARGET_X86_64; ARCH=x86_64
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
    fi
  fi
fi

dnl
dnl FOr now, we only work on x86_64, so check that.
dnl
if test x"$TARGET_CPU" != xTARGET_X86_64 ; then
  AC_MSG_ERROR([sorry, but we only support the x86-64 processor])
else
  CFLAG_M64="-m64"
fi
AC_SUBST(CFLAG_M64)

TARGET=$ARCH-$OS
AC_SUBST(TARGET_CPU)
AC_SUBST(TARGET_OS)
AC_SUBST(TARGET)
AC_SUBST(OS)

dnl
dnl Manticore version information
dnl
case $PACKAGE_VERSION in
  development) VERSION=0.0.0 ;;
  *.*.*) VERSION=$PACKAGE_VERSION ;;
  *.*) VERSION=$PACKAGE_VERSION.0 ;;
  *) AC_MSG_ERROR([badly formed version number \"$PACKAGE_VERSION\"])
  ;;
esac
[MAJOR_VERSION=`echo $VERSION \
  | sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\1/'`]
[MINOR_VERSION=`echo $VERSION \
  | sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\1/'`]
[PATCH_VERSION=`echo $VERSION \
  | sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\1/'`]
MANTICORE_VERSION=$PACKAGE_VERSION
BUILD_DATE=`date +%Y-%m-%d`

dnl BUILD_VERSION and RElEASE_DATE are defined in config/aclocal.m4
dnl by the following macro:
MANTICORE_SET_VERSION

AC_SUBST(MANTICORE_VERSION)
AC_SUBST(MAJOR_VERSION)
AC_SUBST(MINOR_VERSION)
AC_SUBST(PATCH_VERSION)
AC_SUBST(BUILD_DATE)

dnl
dnl check for standard programs
dnl
AC_GNU_SOURCE
AC_PROG_CC_C99
AC_PROG_CXX
AC_CHECK_PROG(AR, ar, ar)
AC_PROG_RANLIB
AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl
dnl extra CFLAGS
dnl
CFLAGS="$CFLAGS -D_GNU_SOURCE -pedantic"

dnl
dnl For various reasons, "gcc -E" doesn't work as a cpp command, so we look for
dnl the old cpp.
dnl
AC_PATH_PROGS(CPP, cpp-4.2 cpp-4.0 cpp, NONE, $PATH:/lib:/usr/lib)
if test x$CPP = x ; then
  AC_MSG_ERROR([no cpp found])
fi

dnl
dnl check for pthreads
dnl
ACX_PTHREAD()

dnl
dnl check for various library routines etc
dnl
AC_CHECK_FUNCS(posix_memalign)
if test x"$ac_cv_func_posix_memalign" != xyes ; then
  AC_CHECK_FUNCS(memalign)
  if test x"$ac_cv_func_posix_memalign" != xyes ; then
    AC_CHECK_FUNCS(valloc)
  fi
fi
dnl
dnl FIXME: clock_gettime requires librt on linux, so we need a special test for it.
dnl
AC_CHECK_FUNCS(mach_absolute_time clock_gettime)
AC_CHECK_TYPES([struct timespec])

AC_CHECK_FUNCS(sigtimedwait nanosleep)

dnl
dnl check for pthread_setaffinity_np
dnl
save_LIBS=$LIBS
save_CFLAGS=$CFLAGS
LIBS="$PTHREAD_LIBS $LIBS"
CFLAGS="$PTHREAD_CFLAGS $CFLAGS"
AC_MSG_CHECKING([for pthread_setaffinity_np])
AC_LINK_IFELSE(
  AC_LANG_PROGRAM(
    [[#include <pthread.h>]],
    [[cpu_set_t s; pthread_setaffinity_np(pthread_self(), sizeof(s), &s); ]]),
  [ ac_cv_func_pthread_setaffinity_np=yes;
    AC_DEFINE(HAVE_PTHREAD_SETAFFINITY_NP, [1], [is pthread_setaffinity_np available?])],
  [ac_cv_func_pthread_setaffinity_np=no])
AC_MSG_RESULT($ac_cv_func_pthread_setaffinity_np)
LIBS=$save_LIBS
CFLAGS=$save_CFLAGS

dnl
dnl check for Linux NUMA support (libnuma)
dnl
AC_CHECK_LIB(numa,numa_available)

dnl check for /proc/cpuinfo
dnl
AC_CHECK_FILES(/proc/cpuinfo)

dnl ******************** SML/NJ configuration ********************
dnl

dnl check that we have a compatible version of SML/NJ
dnl
CHECK_SMLNJ([AC_MSG_ERROR([unable to find SML/NJ installation; check your PATH or set SMLNJ_CMD])])
if test $SMLNJ_MAJOR_VERSION -lt 110 \
  -o $SMLNJ_MINOR_VERSION -lt 65
then
  AC_MSG_ERROR([installation requires SML/NJ version 110.65+])
fi
AC_SUBST(SMLNJ_VERSION)

dnl determine the heap-image suffix
dnl
CHECK_SMLNJ_HEAP_SUFFIX([
  AC_MSG_ERROR([unsupported configuration ${host_cpu}-${host_os}])])

dnl look for ml-makedepend
dnl
AC_MSG_CHECKING([for ml-makedepend])
tmpPATH="$SMLNJ_PATH:$PATH"
AC_PATH_PROG(ML_MAKEDEPEND, ml-makedepend, ":", $tmpPATH)
if test $ML_MAKEDEPEND = none ; then
  AC_MSG_ERROR([ml-makedepend not found])
else
  AC_MSG_RESULT([$ML_MAKEDEPEND])
fi
AC_SUBST(ML_MAKEDEPEND)

dnl
dnl look for ml-build
dnl
tmpPATH="$SMLNJ_PATH:$PATH"
AC_PATH_PROG(ML_BUILD, ml-build, none, $tmpPATH)
if test $ML_BUILD = none ; then
  AC_MSG_ERROR([ml-build not found])
fi
AC_SUBST(ML_BUILD)

dnl
dnl allow the user to override the location of the MLRisc library
dnl
MLRISC_DIR=""
AC_ARG_WITH(mlrisc,
  AC_HELP_STRING([--with-mlrisc=PATH], [specify location of MLRisc library]),
  MLRISC_DIR=$with_mlrisc)
if test x$with_mlrisc = xyes -o x$with_mlrisc = xno ; then
  AC_MSG_ERROR([--with-mlrisc option must specify directory argument])
elif test x$with_mlrisc != x -a ! -r $MLRISC_DIR/cm/MLRISC.cm ; then
  AC_MSG_ERROR([MLRisc library not found at $MLRISC_DIR])
elif test x$MLRISC_DIR != x ; then
dnl
dnl verify that $MLRISC_DIR is an absolute path
dnl
  case $MLRISC_DIR in
    /*) ;;
    *) AC_MSG_ERROR([--with-mlrisc argument must be absolute]) ;;
  esac
fi
if test x$MLRISC_DIR = x ; then
  MLRISC_DIR="<builtin>"
  MLRISC_CM_DIR='$SMLNJ-MLRISC'
else
  MLRISC_CM_DIR=$MLRISC_DIR/cm
fi
AC_SUBST(MLRISC_DIR)
AC_SUBST(MLRISC_CM_DIR)


dnl ******************** Paths ********************

MANTICORE_ROOT=`pwd`
MANTICORE_DOCDIR=$MANTICORE_ROOT/doc
MANTICORE_SRCDIR=$MANTICORE_ROOT/src
MANTICORE_LIBDIR=$MANTICORE_ROOT/lib
MANTICORE_BINDIR=$MANTICORE_ROOT/bin
MANTICORE_MKDIR=$MANTICORE_ROOT/mk

dnl
dnl FIXME: the following should be relative to the MANTICORE_LIBDIR.
dnl
MANTICORE_BASISDIR=$MANTICORE_SRCDIR/lib/basis
MANTICORE_BASISINCDIR=$MANTICORE_SRCDIR/lib/basis
MANTICORE_RUNSRCDIR=$MANTICORE_SRCDIR/lib/parallel-rt

HEAP_IMAGE_DIR=$MANTICORE_BINDIR/.heap
INSTALL_SMLNJ_WRAPPER=$MANTICORE_BINDIR/install-sml-wrapper.sh

AC_SUBST(MANTICORE_ROOT)
AC_SUBST(MANTICORE_DOCDIR)
AC_SUBST(MANTICORE_SRCDIR)
AC_SUBST(MANTICORE_LIBDIR)
AC_SUBST(MANTICORE_BINDIR)
AC_SUBST(MANTICORE_MKDIR)
AC_SUBST(MANTICORE_RUNSRCDIR)
AC_SUBST(MANTICORE_BASISDIR)
AC_SUBST(HEAP_IMAGE_DIR)
AC_SUBST(INSTALL_SMLNJ_WRAPPER)

dnl ******************** Runtime system parameters ********************

AC_DEFINE([MAX_NUM_VPROCS], [16], [maximum number of virtual processors])
dnl VP_HEAP_SZB
dnl MEM_BLOCK_SZB


dnl ******************** C compiler properties ********************

dnl
dnl run the compiler tests in 64-bit mode
dnl
save_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS $CFLAG_M64"

dnl check for leading "_" on global names
dnl
CHECK_LEADING_UNDERSCORE

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

AC_C_BIGENDIAN
AC_C_INLINE
AC_C_CHAR_UNSIGNED
AC_C_LONG_DOUBLE

dnl
dnl Figure out the sizes of a few things; we use this information in
dnl manticore-config.h to determine the type of Word_t.
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
dnl restore CFLAGS
dnl
CFLAGS=$save_CFLAGS

dnl
dnl config.h stuff
AH_TOP([
/*
 * COPYRIGHT (c) 2007 Manticore project. (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _MANTICORE_CONFIG_H_
#define _MANTICORE_CONFIG_H_
])
AH_BOTTOM([

#ifndef NOT_C_SOURCE
typedef unsigned long Word_t;
typedef unsigned long Addr_t;
typedef struct manticore_value *Value_t;
#endif /* !NOT_C_SOURCE */

#endif /* !_MANTICORE_CONFIG_H_ */
])

dnl for the benchmark programs, we need the path to the PML compiler
dnl
PMLC=$MANTICORE_BINDIR/mc
AC_SUBST(PMLC)

dnl for the benchmark programs, we need the path to the MLton compiler
dnl
AC_ARG_WITH(mlton,
  AC_HELP_STRING([--with-mlton=<path>], [specify path to mlton executable]),
  [ac_cv_use_mlton=$withval], [ac_cv_use_mlton=no])
if test x"$ac_cv_use_mlton" = xno ; then
  # look for mlton in the path
  AC_PATH_PROG([MLTON], [mlton], [none],
    [$PATH$PATH_SEPARATOR/usr/bin$PATH_SEPARATOR/usr/local/bin])
else
  AC_MSG_CHECKING([path to mlton ($ac_cv_use_mlton)])
  case $ac_cv_use_mlton in
    /*) ;;
    *) AC_MSG_ERROR([path to mlton must be absolute]) ;;
  esac
  if test -x $ac_cv_use_mlton ; then
    MLTON=$ac_cv_use_mlton
  AC_MSG_RESULT([ okay])
  else
    MLTON=none
  AC_MSG_RESULT([ invalid])
  fi
fi
if test x"$MLTON" = xnone ; then
  AC_MSG_WARN([unable to find mlton])
fi
AC_SUBST(MLTON)

dnl ******************** Output ********************

dnl
dnl generate run-time config files
dnl
AC_CONFIG_FILES(\
dnl ***** Makefiles *****
  Makefile
  src/Makefile
  src/benchmarks/programs/cml-merge/Makefile
  src/benchmarks/programs/cml-parallelpp/Makefile
  src/benchmarks/programs/cml-pingpong/Makefile
  src/benchmarks/programs/cml-primes/Makefile
  src/benchmarks/programs/cml-spawn/Makefile
  src/gen/log-gen/Makefile
  src/lib/parallel-rt/build/Makefile
  src/lib/parallel-rt/build/config/Makefile
  src/lib/parallel-rt/build/mk/common.gmk:src/lib/parallel-rt/build/mk/common_gmk.in
  src/tools/log-dump/Makefile
  src/tools/log-view/build/Makefile
  src/tools/mc/Makefile
dnl ***** SML source files *****
  src/gen/log-gen/main.sml:src/gen/log-gen/main_sml.in
  src/tools/mc/common/run-cpp.sml:src/tools/mc/common/run-cpp_sml.in
  src/tools/mc/common/load-paths.sml:src/tools/mc/common/load-paths_sml.in
  src/tools/mc/driver/build-executable.sml:src/tools/mc/driver/build-executable_sml.in
  src/tools/mc/driver/version-fn.sml:src/tools/mc/driver/version-fn_sml.in
dnl ***** CM files *****
  src/tools/mc/codegen/group.cm:src/tools/mc/codegen/group_cm.in
  src/tools/mc/codegen/wrapper.cm:src/tools/mc/codegen/wrapper_cm.in
)
AC_CONFIG_HEADERS(src/include/manticore-config.h:config/config_h.in)

dnl
dnl shell scripts
dnl
AC_CONFIG_FILES([
  bin/install-sml-wrapper.sh:config/install-sml-wrapper_sh.in
], [chmod +x bin/install-sml-wrapper.sh])

dnl
dnl create machine-specific links
dnl
AC_CONFIG_LINKS([
  src/lib/parallel-rt/machine:src/lib/parallel-rt/$ARCH
  src/lib/parallel-rt/include/machine:src/lib/parallel-rt/include/$ARCH
])

dnl
dnl generate the runtime-constants files
dnl
AC_CONFIG_COMMANDS([runtime-consts], [./config/gen-runtime-consts.sh])

dnl
dnl generate makefiles for runtime library builds
dnl
dnl usage: MK_BUILD_DIR(<build-dir>, <options>)
dnl
AC_DEFUN([MK_BUILD_DIR], [
    if test ! -d src/lib/parallel-rt/build/$1 ; then
      mkdir src/lib/parallel-rt/build/$1
    fi
    [sed -e 's,@BUILD_TARGET@,$1,g' -e 's,@BUILD_OPTIONS@,$2,g' \
      src/lib/parallel-rt/build/mk/Makefile.in \
        > src/lib/parallel-rt/build/$1/Makefile.in]
    AC_CONFIG_FILES(src/lib/parallel-rt/build/$1/Makefile)
  ])

if test $CROSS_COMPILING = 0 ; then
    MK_BUILD_DIR(mcrt, [])
    MK_BUILD_DIR(mcrt-debug, [debug])
    MK_BUILD_DIR(mcrt-log, [logging])
    MK_BUILD_DIR(mcrt-log-debug, [logging debug])
fi

AC_OUTPUT

