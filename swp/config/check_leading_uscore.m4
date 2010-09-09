dnl check_leading_uscore.m4
dnl
dnl COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
dnl All rights reserved.
dnl
dnl @synopsis CHECK_LEADING_UNDERSCORE
dnl
dnl This macro tests to see if the system prefixes a "_" to global
dnl names.
AC_DEFUN(CHECK_LEADING_UNDERSCORE, [
dnl
dnl check for the nm program
dnl
  AC_PATH_PROGS(NM, nm, none)
  if test x$NM = z ; then
    AC_MSG_ERROR([unable to find nm])
  else
    AC_MSG_CHECKING([for leading underscore in global names])
    AC_LANG_PUSH(C)
    AC_LANG_CONFTEST([[void aBcDeF () { }]])
    $CC -c conftest.c
    AC_LANG_POP()
    if test -r conftest.o ; then
      $NM conftest.o | grep -w _aBcDeF > /dev/null
      if test $? = 0 ; then
        cv_check_leading_underscore=yes
        AC_MSG_RESULT([yes])
        AC_DEFINE(GLOBALS_HAVE_UNDERSCORE, 1, [Global symbols have a leading underscore])
      else
        cv_check_leading_underscore=no
        AC_MSG_RESULT([no])
      fi
      rm -f conftest.o conftest.c
    else
      AC_MSG_ERROR([compile of conftest.c failed])
    fi
  fi
])dnl
