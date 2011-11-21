dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2009 The Manticore Project (manticore.cs.uchicago.edu)
dnl All rights reserved.
dnl

AC_INIT(manticore-benchmarks,1.0,http://manticore.cs.uchicago.edu)

AC_PREREQ(2.59)
AC_CONFIG_AUX_DIR(config)

dnl
dnl include extra macros
dnl
sinclude(config/check_smlnj.m4)
sinclude(config/check_smlnj_heap_suffix.m4)

AC_CANONICAL_HOST

AC_PROG_INSTALL

dnl check for mlton
dnl
AC_PATH_PROG([MLTON], [mlton], [none],
  [$PATH$PATH_SEPARATOR/usr/bin$PATH_SEPARATOR/usr/local/bin])
AC_SUBST(MLTON)

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
dnl CHECK_SMLNJ_HEAP_SUFFIX([
dnl   AC_MSG_ERROR([unsupported configuration ${host_cpu}-${host_os}])])
SMLNJ_ARCH=x86
SMLNJ_OPSYS=darwin

dnl ******************** Paths ********************

MANTI_BENCH_ROOT=`pwd`
MANTI_BENCH_SCRIPTS=$MANTI_BENCH_ROOT/scripts
MANTI_BENCH_RESULTS=$MANTI_BENCH_ROOT/../results

AC_SUBST(MANTI_BENCH_ROOT)
AC_SUBST(MANTI_BENCH_SCRIPTS)
AC_SUBST(MANTI_BENCH_RESULTS)

MANTICORE_ROOT=""
AC_ARG_WITH(manticore,
  AS_HELP_STRING([--with-manticore=PATH], [specify location of Manticore root directory]),
  [MANTICORE_ROOT=$with_manticore])
if test x$with_manticore = xyes -o x$with_manticore = xno ; then
  AC_MSG_ERROR([--with-manticore option must specify directory argument])
elif test x$MANTICORE_ROOT != x ; then
dnl
dnl verify that $MANTICORE_ROOT is an absolute path
dnl
  case $MANTICORE_ROOT in
    /*) ;;
    *) AC_MSG_ERROR([--with-MANTICORE argument must be absolute]) ;;
  esac
fi

AC_ARG_WITH(resultsdir,
  AS_HELP_STRING([--with-resultsdir=PATH], [specify location of benchmarking results directory]),
  [MANTI_BENCH_RESULTS=$with_resultsdir])
if test x$with_resultsdir = xyes -o x$with_resultsdir = xno ; then
  AC_MSG_ERROR([--with-resultsdir option must specify directory argument])
elif test x$MANTI_BENCH_RESULTS != x ; then
dnl
dnl verify that $MANTI_BENCH_RESULTS is an absolute path
dnl
  case $MANTI_BENCH_RESULTS in
    /*) ;;
    *) AC_MSG_ERROR([--with-resultsdir argument must be absolute]) ;;
  esac
fi

PMLCFLAGS=""
AC_ARG_WITH(pmlcflags,
  AS_HELP_STRING([--with-pmlcflags=flags], [Manticore compiler flags]),
  [PMLCFLAGS=$with_pmlcflags])

GEN_GC_STATS="false"
AC_ARG_WITH(gcstats,
  AS_HELP_STRING([--with-gcstats], [generate gc stats]),
  [PMLCFLAGS="$PMLCFLAGS -gcstats"; GEN_GC_STATS="true"])

MANTICORE_BINDIR=$MANTICORE_ROOT/bin
PMLC=$MANTICORE_BINDIR/pmlc
LOG_WORK_STEALING=$MANTICORE_ROOT/src/tools/log-work-stealing/log-work-stealing

AC_SUBST(MANTICORE_ROOT)
AC_SUBST(PMLC)
AC_SUBST(LOG_WORK_STEALING)
AC_SUBST(PMLCFLAGS)
AC_SUBST(GEN_GC_STATS)
AC_SUBST(SMLNJ_CMD)

AC_CONFIG_FILES(\
dnl ***** Makefiles *****
  programs/cml-pingpong/Makefile
)

dnl ***** Scripts ******
AC_CONFIG_FILES(
  [scripts/gen-makefile.sh:scripts/gen_makefile_sh.in],
  [chmod +x scripts/gen-makefile.sh])
AC_CONFIG_FILES(
  [scripts/gen-all-makefiles.sh:scripts/gen_all_makefiles_sh.in],
  [chmod +x scripts/gen-all-makefiles.sh])
AC_CONFIG_FILES(
  [scripts/gen-input-data.sh:scripts/gen_input_data_sh.in],
  [chmod +x scripts/gen-input-data.sh])
AC_CONFIG_FILES(
  [scripts/gen-run.sh:scripts/gen_run_sh.in],
  [chmod +x scripts/gen-run.sh])
AC_CONFIG_FILES(
  [scripts/experiment.sh:scripts/experiment_sh.in],
  [chmod +x scripts/experiment.sh])
AC_CONFIG_FILES(
  [scripts/gen-rope-split-experiment.sh:scripts/gen_rope_split_experiment_sh.in],
  [chmod +x scripts/gen-rope-split-experiment.sh])
AC_CONFIG_FILES(
  [scripts/gen-leaf-size-experiment.sh:scripts/gen_leaf_size_experiment_sh.in],
  [chmod +x scripts/gen-leaf-size-experiment.sh])
AC_CONFIG_FILES(
  [scripts/gen-speedup-experiment.sh:scripts/gen_speedup_experiment_sh.in],
  [chmod +x scripts/gen-speedup-experiment.sh])
AC_CONFIG_FILES(
  [scripts/gen-sst-experiment.sh:scripts/gen_sst_experiment_sh.in],
  [chmod +x scripts/gen-sst-experiment.sh])
AC_CONFIG_FILES(
  [scripts/work-stealing-stats.sh:scripts/work_stealing_stats_sh.in],
  [chmod +x scripts/work-stealing-stats.sh])
AC_CONFIG_FILES(
  [stats/work-stealing/gen-work-stealing-stats.sh:stats/work-stealing/gen_work_stealing_stats_sh.in],
  [chmod +x stats/work-stealing/gen-work-stealing-stats.sh])
AC_CONFIG_FILES(
  [scripts/profile-rope-split.sh:scripts/gen_profile_rope_split_sh.in],
  [chmod +x scripts/profile-rope-split.sh])
AC_CONFIG_FILES(
  [scripts/experiment-mlton.sh:scripts/experiment_mlton_sh.in],
  [chmod +x scripts/experiment-mlton.sh])

AC_OUTPUT
