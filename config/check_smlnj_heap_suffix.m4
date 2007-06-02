dnl check_smlnj_heap_suffix.m4
dnl
dnl COPYRIGHT (c) 2006 The SML/NJ Fellowship.
dnl
dnl @synopsis CHECK_SMLNJ_HEAP_SUFFIX(ACTION-IF-UNKNOWN)
dnl
dnl This macro figures out the suffix of heap-image files used by SML/NJ
dnl run-time on the host architecture and operating system.  If the architecture
dnl and/or operating system is not one supported by SML/NJ, then it executes
dnl the ACTION-IF-UNKNOWN.  Upon successful execution, this macro defines the
dnl shell variables SMLNJ_ARCH, SMLNJ_OPSYS, and SMLNJ_HEAP_SUFFIX, and it
dnl does an AC_SUBST on these variables.
dnl 
dnl @version $Id: check_smlnj_heap_suffix.m4,v 1.2 2007/05/09 17:48:52 jhr Exp $
dnl @author John Reppy <http://www.cs.uchicago.edu/~jhr>
dnl
AC_DEFUN(CHECK_SMLNJ_HEAP_SUFFIX, [
  AC_REQUIRE([AC_CANONICAL_HOST])
  case ${host_cpu}:${host_os} in
    alpha*:osf3*)	SMLNJ_ARCH=alpha32x;	SMLNJ_OPSYS=osf1;;
    alpha*:osf4*)	SMLNJ_ARCH=alpha32;	SMLNJ_OPSYS=dunix;;
    hppa*:hpux9*)	SMLNJ_ARCH=hppa;	SMLNJ_OPSYS=hpux9;;
    hppa*:hpux10*)	SMLNJ_ARCH=hppa;	SMLNJ_OPSYS=hpux;;
    i386:darwin*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=darwin;;
    i386:freebsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=freebsd;;
    i486:freebsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=freebsd;;
    i586:freebsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=freebsd;;
    i686:freebsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=freebsd;;
    i386:linux*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=linux;;
    i486:linux*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=linux;;
    i586:linux*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=linux;;
    i686:linux*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=linux;;
    i386:netbsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=netbsd;;
    i486:netbsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=netbsd;;
    i586:netbsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=netbsd;;
    i686:netbsd*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=netbsd;;
    i386:solaris3)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=solaris;;
    mips:irix4*)	SMLNJ_ARCH=mipseb;	SMLNJ_OPSYS=irix4; HEAP_OPSYS=irix;;
    mips:irix5*)	SMLNJ_ARCH=mipseb;	SMLNJ_OPSYS=irix5; HEAP_OPSYS=irix;;
    mips:irix6*)	SMLNJ_ARCH=mipseb;	SMLNJ_OPSYS=irix6; HEAP_OPSYS=irix;;
    powerpc:aix*)	SMLNJ_ARCH=ppc;		SMLNJ_OPSYS=aix;;
    powerpc:darwin*)	SMLNJ_ARCH=ppc;		SMLNJ_OPSYS=darwin;;
    rs6000:aix*)	SMLNJ_ARCH=rs6000;	SMLNJ_OPSYS=aix;;
    sparc:solaris2*)	SMLNJ_ARCH=sparc;	SMLNJ_OPSYS=sunos;;
    sparc:solaris3*)	SMLNJ_ARCH=sparc;	SMLNJ_OPSYS=solaris;;
    x86_64:linux*)	SMLNJ_ARCH=x86;		SMLNJ_OPSYS=linux;;
    *) $1 ;;
  esac
  if test z$SMLNJ_ARCH != z -a z$SMLNJ_OPSYS != z ; then
    if test z$HEAP_OPSYS = z ; then
      SMLNJ_HEAP_SUFFIX=${SMLNJ_ARCH}-${SMLNJ_OPSYS}
    else
      SMLNJ_HEAP_SUFFIX=${SMLNJ_ARCH}-${HEAP_OPSYS}
    fi
    AC_SUBST(SMLNJ_ARCH)
    AC_SUBST(SMLNJ_OPSYS)
    AC_SUBST(SMLNJ_HEAP_SUFFIX)
  fi
])dnl
