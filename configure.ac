dnl BANNERSTART
dnl - Copyright 2006-2008, Galois, Inc.
dnl - This software is distributed under a standard, three-clause BSD license.
dnl - Please see the file LICENSE, distributed with this software, for specific
dnl - terms and conditions.
dnl Author: Adam Wick <awick@galois.com>
dnl BANNEREND

MESSAGE_LOG=
FAILED=No

AC_DEFUN([LOG_FAILURE], [
  MESSAGE_LOG=${MESSAGE_LOG}"-IT- ERROR: "$1
  FAILED=Yes
 ])

AC_DEFUN([LOG_FAILUREC], [
  MESSAGE_LOG=${MESSAGE_LOG}"-EBR-"$1
 ])

AC_DEFUN([LOG_WARNING], [
  MESSAGE_LOG=${MESSAGE_LOG}"-IT- WARNING: "$1
 ]) 

AC_DEFUN([LOG_WARNINGC], [
  MESSAGE_LOG=${MESSAGE_LOG}"-WBR-"$1
 ])

AC_DEFUN([AC_FORCE_EXIST], [
  AC_PATH_PROGS([$1], [$2], no)
  if test "$$1" == no; then
    LOG_FAILURE("Couldn't find required program $2")
  fi
])

AC_DEFUN([AC_TESTS_WANT_EXIST], [
  AC_PATH_PROGS([$1], [$2], no)
  if test "$$1" == no; then
    LOG_WARNING("Couldn't find required program $2 (needed to run test suite)")
  fi
])

AC_DEFUN([AC_CHECK_PROG_VER],
[AC_PATH_PROGS([$1], [$2])
if test -z "[$]$1"; then
  ac_verc_fail=yes
else
  # Found it, now check the version.
  AC_MSG_CHECKING([version of [$]$1])
changequote(<<,>>)dnl
  ac_prog_version=`<<$>>$1 $3 2>&1 ifelse(<<$4>>,,,
                   <<| sed -n 's/^.*patsubst(<<$4>>,/,\/).*$/\1/p'>>)`
  case $ac_prog_version in
    '') ac_prog_version="v. ?.??, bad"; ac_verc_fail=yes;;
    <<$5>>)
changequote([,])dnl
       ac_prog_version="$ac_prog_version, ok"; ac_verc_fail=no;;
    *) ac_prog_version="$ac_prog_version, bad"; ac_verc_fail=yes;;
  esac
  AC_MSG_RESULT([$ac_prog_version])
fi
ifelse([$6],,,
[if test $ac_verc_fail = yes; then
  $6
fi])
])

AC_PREREQ(2.53)
AC_INIT(HALVM,1.0,awick@galois.com,halvm)

dnl Check for sed and echo first, with an abrupt failure if they're not found,
dnl because we need these to show output.
AC_PATH_PROGS(SED, sed, no)
if test "$SED" == no; then
  AC_MSG_ERROR("The HaLVM requires sed to build.")
fi
AC_PATH_PROGS(ECHO, echo, no)
if test "$ECHO" == no; then
  AC_MSG_ERROR("The HaLVM requires echo to build.")
fi

dnl From this point on, we should use LOG_FAILURE instead of AC_FORCE_EXIST.
AC_FORCE_EXIST(RM,rm)
AC_FORCE_EXIST(CP,cp)
AC_FORCE_EXIST(MV,mv)
AC_FORCE_EXIST(LN,ln)
AC_FORCE_EXIST(LD,ld)
AC_FORCE_EXIST(AR,ar)
AC_FORCE_EXIST(CAT,cat)
AC_FORCE_EXIST(CPP,cpp)
AC_FORCE_EXIST(TAR,tar)
AC_FORCE_EXIST(WGET,wget)
AC_FORCE_EXIST(FIND,find)
AC_FORCE_EXIST(GREP,grep)
AC_FORCE_EXIST(MAKE,make)
AC_FORCE_EXIST(PERL,perl)
AC_FORCE_EXIST(TAIL,tail)
AC_FORCE_EXIST(CHMOD,chmod)
AC_FORCE_EXIST(MKDIR,mkdir)
AC_FORCE_EXIST(PATCH,patch)
AC_FORCE_EXIST(TOUCH,touch)
AC_FORCE_EXIST(XARGS,xargs)
AC_FORCE_EXIST(AUTORECONF,autoreconf)
AC_FORCE_EXIST(AUTOCONF,autoconf)

AC_TESTS_WANT_EXIST(XM,xm)
AC_TESTS_WANT_EXIST(DIFF,diff)
AC_TESTS_WANT_EXIST(QEMU,qemu)
AC_TESTS_WANT_EXIST(SUDO,sudo)
AC_TESTS_WANT_EXIST(CABAL,cabal)
AC_TESTS_WANT_EXIST(KILLALL,killall)

dnl Figure out whether or not the compiler accepts -fno-unit-at-a-time
AC_LANG_PUSH(C)
AC_CACHE_CHECK([whether gcc has -fno-unit-at-a-time], [fp_cv_gcc_has_no_unit_at_a_time],
[OLD_CFLAGS=$CFLAGS
 CFLAGS="${CFLAGS} -fno-unit-at-a-time"
 AC_COMPILE_IFELSE([AC_LANG_SOURCE([[int main(int a, char *b){ return 1; }]])],
   [fp_cv_gcc_has_no_unit_at_a_time=yes],
   [fp_cv_gcc_has_no_unit_at_a_time=no])
 CFLAGS=$OLD_CFLAGS])

if test "$fp_cv_gcc_has_no_unit_at_a_time" = "yes"; then
  AC_SUBST(NO_UNIT_AT_A_TIME_OPT,"-fno-unit-at-a-time")
fi
AC_LANG_POP(C)

dnl Figure out whether or not the compiler accepts -fno-stack-protector
AC_LANG_PUSH(C)
AC_CACHE_CHECK([whether gcc has -fno-stack-protector], [fp_cv_gcc_has_no_stack_protector],
[OLD_CFLAGS=$CFLAGS
 CFLAGS="${CFLAGS} -fno-stack-protector"
 AC_COMPILE_IFELSE([AC_LANG_SOURCE([[int main(int a, char *b){ return 1; }]])],
   [fp_cv_gcc_has_no_stack_protector=yes],
   [fp_cv_gcc_has_no_stack_protector=no])
 CFLAGS=$OLD_CFLAGS])

if test "$fp_cv_gcc_has_no_stack_protector" = "yes"; then
  AC_SUBST(NO_STACK_PROTECTOR_OPT,"-fno-stack-protector")
  CFLAGS="${CFLAGS} -fno-stack-protector"
fi
AC_LANG_POP(C)

dnl Figure out if we have a sufficiently recent version of glibc
AC_LANG_PUSH(C)
AC_CACHE_CHECK([whether glibc version >= 2.4], [fp_cv_glibc_better_then_24],
  [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <features.h>

int main(int a, char *b) {
  return !((__GLIBC__ >= 2) && (__GLIBC_MINOR__ >= 4));
}
]])],
  [fp_cv_glibc_better_than_24=yes
   echo -n "yes"],
  [fp_cv_glibc_better_than_24=no
   echo -n "no"]]))
AC_LANG_POP(C)

if test "$fp_cv_glibc_better_than_24" = "no"; then
  LOG_FAILURE("The HaLVm requires GLIBC >= 2.4.")
fi

dnl Make sure we have gmp.so.3
AC_LANG_PUSH(C)
AC_CHECK_LIB(gmp, __gmpz_mul_si, [], LOG_FAILURE("The HaLVM requires GMP (version 3) to be installed."))
AC_LANG_POP(C)

dnl See if we have the FUSE library
AC_LANG_PUSH(C)
OLDCPPFLAGS="${CPPFLAGS}"
CPPFLAGS="${CPPFLAGS} -D_FILE_OFFSET_BITS=64"
AC_CHECK_HEADER(fuse.h,[AC_SUBST(HAVE_FUSE_H,yes)],[
  LOG_WARNING("You don't appear to have the FUSE library. This isn't a")
  LOG_WARNINGC(["problem per se, but will make it much more difficult to"])
  LOG_WARNINGC("develop domains that have a file system. We recommend you")
  LOG_WARNINGC("add the FUSE library - usually fuse-devel or similar - before")
  LOG_WARNINGC("continuing.")
  AC_SUBST(HAVE_FUSE_H,no)])
CPPFLAGS="${OLDCPPFLAGS}"
AC_LANG_POP(C)

AC_ARG_ENABLE(bytecode-interpreter,
 [AS_HELP_STRING([--enable-bytecode-interpreter],
   [include support for the GHC runtime to execute bytecode])],
 [AC_SUBST(BYTECODE_INTERP_OPT, "-optc-DALLOW_INTERPRETER")],
 [AC_SUBST(BYTECODE_INTERP_OPT, "")])

AC_ARG_ENABLE(sanity-checker,
 [AS_HELP_STRING([--enable-sanity-checker],
   [include support for running the GHC sanity checker outside debug mode])],
 [AC_SUBST(SANITY_CHECKER_OPT, "-optc-DPERFORM_SANITY_CHECKS")],
 [AC_SUBST(SANITY_CHECKER_OPT, "")])

dnl Now check for the Xen functionality we want.

AC_ARG_WITH(xen-tree, 
 [AS_HELP_STRING([--with-xen-tree=ARG],
   [build against the given Xen source tree])],
 [INCLUDE_DIRS="$withval"
  CURDIR=`pwd`
  CPPFLAGS="${CPPFLAGS} -I${withval}/dist/install/usr/include"
  AC_SUBST(XEN_INCLUDE_DIRECTORY,"${withval}/dist/install/usr/include")
  AC_SUBST(XEN_BIN_ROOT,"${withval}/dist/install/")],
 [AC_SUBST(XEN_BIN_ROOT,"/")
  AC_SUBST(XEN_INCLUDE_DIRECTORY,"/usr/include")])

dnl First, see if we have Xen around.
AC_CHECK_HEADER(xen/xen.h,[],[
  LOG_FAILURE("Couldn't find the Xen headers. Please either install them")
  LOG_FAILUREC(["(they're probably in something like xen-devel), or use the"])
  LOG_FAILUREC(["--with-xen-tree configure flag to point this script at the"])
  LOG_FAILUREC(["base of a Xen source repository checkout."])])

dnl Is this a 64-bit platform?
if test `uname -m` = "x86_64"; then
  memory_model="64";
  AC_SUBST(ARCHITECTURE,"x86_64")
  AC_SUBST(ABI,64)
else
  AC_SUBST(ARCHITECTURE,"i386")
  AC_SUBST(ABI,32)
fi

if test "$memory_model" != "64"; then
  AC_ARG_ENABLE(pae,
   [AS_HELP_STRING([--enable-pae],
     [use the Intel PAE memory model. This is the default.])],
   [if test "$enableval" = "yes"; then
      memory_model="32p"
    else
      memory_model="32"
    fi ],
   [memory_model="32p"])
fi

AC_CHECK_DECL(xc_gnttab_open, 
 [AC_SUBST(BUILD_LIBIVC, "y")],
 [AC_SUBST(BUILD_LIBIVC, "n")],
 [#include <xenctrl.h>])

case "$memory_model" in
  "32")  AC_SUBST(ARCH_OPTION, "-DCONFIG_X86_32")
	 AC_SUBST(ARCH_FLAGS,  "")
         MEMMODEL_STR="32-bit" ;
         ARCH=i386 ;;
  "32p") AC_SUBST(ARCH_OPTION, "-DCONFIG_X86_PAE")
	 AC_SUBST(ARCH_FLAGS,  "")
         MEMMODEL_STR="PAE (36-bit physical / 32-bit virtual)" ;
         ARCH=i386 ;;
  "64")  AC_SUBST(ARCH_OPTION, "-DCONFIG_X86_64")
	 AC_SUBST(ARCH_FLAGS,  "-fomit-frame-pointer -fno-asynchronous-unwind-tables -fno-builtin -mno-red-zone")
         MEMMODEL_STR="64-bit" ;
         ARCH=x86_64 ;;
  *)     LOG_FAILURE("Unknown memory model (internal error?)") ;;
esac

ghc_download_name=ghc-6.12.3-${ARCH}-unknown-linux-n.tar.bz2
AC_SUBST(GHC_BINARY_NAME,$ghc_download_name)

if test "${MESSAGE_LOG}" != ""; then
  echo ${MESSAGE_LOG} | sed -e "s/-IT-/\n /g" -e "s/-WBR-/\n           /g" -e "s/-EBR-/\n         /g"
fi
echo

dnl Set the TOPDIR for the HaLVM build system; makes it viable to include
dnl common.mk from anywhere
CURDIR=`pwd`
AC_SUBST(TOPDIR,${CURDIR})

dnl Build with GMP?
AC_ARG_ENABLE([gmp],
  [AC_HELP_STRING([--enable-gmp],
                  [Build with gmp instead of integer-simple])],
  [use_gmp=true], [])
AC_SUBST(USE_GMP, [$use_gmp])

if test "${FAILED}" == "Yes"; then
  echo "Fix these errors, and then try again."
else
  echo "  HaLVM Memory Model: ${MEMMODEL_STR}"
  echo
  AC_CONFIG_FILES([mk/common.mk
                   static-bits/examples/mk/tests.mk
                   static-bits/examples/mk/rules.mk
                   libraries/HALVMCore/HALVMCore.buildinfo
                   libraries/XenDevice/XenDevice.buildinfo
                   libraries/libDomainBuilder/libDomainBuilder.buildinfo
		   libraries/communication/communication.buildinfo
                   libIVC/Makefile
                   libm/Makefile])
  AC_OUTPUT
fi
