dnl check_smlnj.m4
dnl
dnl COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
dnl All rights reserved.
dnl
dnl @synopsis CHECK_SMLNJ(ACTION-IF-UNKNOWN)
dnl
dnl This macro figures out the location of SML/NJ and its major, minor,
dnl and patch version numbers.  The shell variables SMLNJ_CMD, SMLNJ_PATH,
dnl SMLNJ_VERSION,  SMLNJ_MAJOR_VERSION, SMLNJ_MINOR_VERSION, and
dnl SMLNJ_PATCH_VERSION are set by this macro when it executes successfully.
dnl This macro also does an AC_SUBST for SMLNJ_CMD and SMLNJ_PATH.
dnl You can override the version of SML/NJ used by defining the SMLNJ_CMD
dnl variable in the environment.
dnl 
dnl @version $Id: check_smlnj.m4,v 1.1.1.1 2006/04/11 05:17:06 jhr Exp $
dnl @author John Reppy <jhr@cs.uchicago.edu>
dnl
AC_DEFUN(CHECK_SMLNJ, [
dnl
dnl first we check for the existence of SML/NJ
dnl
  if test z$SMLNJ_CMD = z ; then
    AC_PATH_PROGS(SMLNJ_CMD, sml-cm sml, none)
  fi
  if test $SMLNJ_CMD = none; then
    $1
  else
dnl
dnl SML/NJ is installed, so determine its location
dnl
    SMLNJ_PATH=`dirname $SMLNJ_CMD`
dnl
dnl Determine the version numbers
dnl
    AC_MSG_CHECKING([version of SML/NJ])
    ac_check_smlnj_version=`$SMLNJ_CMD @SMLversion`
    if test $? -eq 0 ; then
dnl
dnl normalize the ac_check_smlnj_version variable
dnl
      case $ac_check_smlnj_version in
	sml-cm*) ac_check_smlnj_version=`echo $ac_check_smlnj_version | sed -e 's/sml-cm //'` ;;
	sml*) ac_check_smlnj_version=`echo $ac_check_smlnj_version | sed -e 's/sml //'` ;;
	*) AC_MSG_ERROR([bogus SML/NJ version ($ac_check_smlnj_version) reported]);;
      esac
      SMLNJ_VERSION=$ac_check_smlnj_version
      case $ac_check_smlnj_version in
	110)
dnl
dnl Versions 110.0.x report "sml 110" for the @SMLversion flag, so we need to
dnl do some more work.
dnl
	  banner=`echo "" | $SMLNJ_CMD | head -1`
	  [ac_check_smlnj_version=`echo $banner \
	    | sed -e 's/.*Version \([0-9.]*\).*/\1/'`]
	  SMLNJ_VERSION=$ac_check_smlnj_version
	  ;;
	*.*.*) ;;
	*.*) ac_check_smlnj_version="$ac_check_smlnj_version".0 ;;
	*) ac_check_smlnj_version="$ac_check_smlnj_version".0.0 ;;
      esac
      [SMLNJ_MAJOR_VERSION=`echo $ac_check_smlnj_version \
	| sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\1/'`]
      [SMLNJ_MINOR_VERSION=`echo $ac_check_smlnj_version \
	| sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\2/'`]
      [SMLNJ_PATCH_VERSION=`echo $ac_check_smlnj_version \
	| sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\3/'`]
      AC_MSG_RESULT([$SMLNJ_VERSION])
      AC_SUBST(SMLNJ_CMD)
      AC_SUBST(SMLNJ_PATH)
    else
      $1
    fi
  fi
])dnl
