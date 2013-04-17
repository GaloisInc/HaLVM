dnl BANNERSTART
dnl - Copyright 2006-2008, Galois, Inc.
dnl - This software is distributed under a standard, three-clause BSD license.
dnl - Please see the file LICENSE, distributed with this software, for specific
dnl - terms and conditions.
dnl Author: Adam Wick <awick@galois.com>
dnl BANNEREND
AC_PREREQ(2.60)
AC_INIT(HALVM,2.0.0,awick@galois.com,halvm)

dnl -------------------------------------------------------------------------
dnl
dnl Message logging system for autoconf. Allows us to stack up errors and
dnl warnings and print them all in a lump at the end.
dnl
dnl -------------------------------------------------------------------------

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

dnl -------------------------------------------------------------------------
dnl
dnl Basic program requirements: If these don't exist, then the rest of our
dnl autoconf system won't work.
dnl
dnl -------------------------------------------------------------------------


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

dnl -------------------------------------------------------------------------
dnl
dnl Other program requirements: If these don't exist, then the autoconf
dnl script will run correctly, but we either we won't be able to build
dnl anything or we'll have to disable some portion of the system.
dnl
dnl -------------------------------------------------------------------------

dnl From this point on, we should use LOG_FAILURE instead of AC_FORCE_EXIST.
AC_FORCE_EXIST(INSTALL,install)
AC_FORCE_EXIST(CC,gcc)
AC_FORCE_EXIST(LD,ld)
AC_FORCE_EXIST(NM,nm)
AC_FORCE_EXIST(RM,rm)
dnl AC_FORCE_EXIST(CP,cp)
dnl AC_FORCE_EXIST(MV,mv)
dnl AC_FORCE_EXIST(LN,ln)
dnl AC_FORCE_EXIST(AR,ar)
dnl AC_FORCE_EXIST(CAT,cat)
dnl AC_FORCE_EXIST(CPP,cpp)
AC_CHECK_PROG_VER(GHC, ghc, --version, [version \([0-9.]*\)], [[789].*],
                  LOG_FAILURE("The HaLVM requires GHC 7 or higher."))
AC_FORCE_EXIST(GIT,git)
dnl AC_FORCE_EXIST(TAR,tar)
AC_CHECK_PROG_VER(ALEX, alex, --version, [version \([0-9.]*\)],
                  [3.[0-9]* | 2.3[0-9.]*],
                  LOG_FAILURE("The HaLVM requires alex 2.2+."))
dnl                  [\(2.[23456789].*\)\|\([3456789].*\)],
dnl AC_FORCE_EXIST(WGET,wget)
dnl AC_FORCE_EXIST(FIND,find)
dnl AC_FORCE_EXIST(GREP,grep)
dnl AC_FORCE_EXIST(MAKE,make)
dnl AC_FORCE_EXIST(PERL,perl)
dnl AC_FORCE_EXIST(TAIL,tail)
dnl AC_FORCE_EXIST(CHMOD,chmod)
dnl AC_FORCE_EXIST(MKDIR,mkdir)
dnl AC_FORCE_EXIST(PATCH,patch)
AC_CHECK_PROG_VER(HAPPY, happy, --version, [Version \([0-9.]*\)], [1.1[89].*],
                  LOG_FAILURE("The HaLVM requires Happy 1.16+."))
AC_FORCE_EXIST(TOUCH,touch)
dnl AC_FORCE_EXIST(XARGS,xargs)
AC_FORCE_EXIST(OBJDUMP,objdump)
AC_FORCE_EXIST(AUTORECONF,autoreconf)
dnl AC_FORCE_EXIST(AUTOCONF,autoconf)
dnl 
dnl AC_TESTS_WANT_EXIST(XM,xm)
dnl AC_TESTS_WANT_EXIST(DIFF,diff)
dnl AC_TESTS_WANT_EXIST(QEMU,qemu)
dnl AC_TESTS_WANT_EXIST(SUDO,sudo)
dnl AC_TESTS_WANT_EXIST(CABAL,cabal)
dnl AC_TESTS_WANT_EXIST(KILLALL,killall)

dnl -------------------------------------------------------------------------
dnl
dnl C Compiler Oddities: Here we check for a few different flags that we
dnl may need to pass in order fo the HaLVM to run correctly.
dnl
dnl -------------------------------------------------------------------------
AC_LANG_PUSH(C)

AC_DEFUN([GCC_FLAG_CHECK], [
    AC_CACHE_CHECK([whether gcc has $1], [$2], [
      OLD_CFLAGS=${CFLAGS}
      CFLAGS="${CFLAGS} $1"
      AC_COMPILE_IFELSE([AC_LANG_SOURCE([[int main(int a,char*b){return 1;}]])],
        [$2=yes],
        [$2=no])
      CFLAGS=${OLD_CFLAGS}
    ])

    if test "$$2" = "yes"; then
      AC_SUBST($3,"$1")
    else
      AC_SUBST($3,"")
    fi
  ])

GCC_FLAG_CHECK(-fno-unit-at-a-time,fp_cv_gcc_has_no_unit_at_a_time,
               NO_UNIT_AT_A_TIME_OPT)
GCC_FLAG_CHECK(-fno-stack-protector,fp_cv_gcc_has_no_stack_protector,
               NO_STACK_PROT_OPT)
GCC_FLAG_CHECK(-fomit-frame-pointer,fp_cv_gcc_has_omit_frame_pointer,
               NO_FRAME_PTR_OPT)
GCC_FLAG_CHECK(-fno-asynchronous-unwind-tables,fp_cv_gcc_has_no_async_unwind,
               NO_ASYNC_UNWIND_OPT)
GCC_FLAG_CHECK(-mno-red-zone,fp_cv_gcc_has_no_red_zone,
               NO_RED_ZONE_OPT)
GCC_FLAG_CHECK(-fno-builtin,gp_cv_gcc_has_no_builtin,
               NO_BUILTIN_OPT)

AC_LANG_POP(C)

dnl Build with GMP?
int_library=integer-simple
AC_ARG_ENABLE([gmp],
  [AC_HELP_STRING([--enable-gmp],
                  [Build with gmp instead of integer-simple])],
  [int_library=integer-gmp])
AC_SUBST(INTEGER_LIBRARY, [$int_library])

if test "$int_library" = "integer-gmp"; then
  AC_SUBST(USE_GMP, [yes])
else
  AC_SUBST(USE_GMP, [no])
fi


dnl -------------------------------------------------------------------------
dnl
dnl GHC Config
dnl
dnl -------------------------------------------------------------------------

AC_ARG_ENABLE(docs,
  [AS_HELP_STRING([--enable-docs], [build documentation])],
  [if test "$enableval" = "yes"; then
     enable_docs="yes"
   else
     enable_docs="no"
   fi
  ], [enable_docs="no"])
AC_SUBST(ENABLE_DOCS, [$enable_docs])

dnl -------------------------------------------------------------------------
dnl
dnl Xen Stuff
dnl
dnl -------------------------------------------------------------------------

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
  arch="x86_64"
  abi="32"
else
  arch="i368"
  abi="32"
fi

AC_SUBST(ARCHITECTURE, "$arch")
AC_SUBST(ABI, "$abi")
AC_SUBST(target,"${arch}-unknown-HaLVM")
AC_SUBST(target_prefix,"${arch}-unknown-HaLVM-")

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
         MEMMODEL_STR="32-bit" ;
         ARCH=i386 ;;
  "32p") AC_SUBST(ARCH_OPTION, "-DCONFIG_X86_PAE")
         MEMMODEL_STR="PAE (36-bit physical / 32-bit virtual)" ;
         ARCH=i386 ;;
  "64")  AC_SUBST(ARCH_OPTION, "-DCONFIG_X86_64")
         MEMMODEL_STR="64-bit" ;
         ARCH=x86_64 ;;
  *)     LOG_FAILURE("Unknown memory model (internal error?)") ;;
esac


dnl -------------------------------------------------------------------------
dnl
dnl Final message and file dump
dnl
dnl -------------------------------------------------------------------------

if test "${MESSAGE_LOG}" != ""; then
  echo ${MESSAGE_LOG} | sed -e "s/-IT-/\n /g" -e "s/-WBR-/\n           /g" -e "s/-EBR-/\n         /g"
fi

if test "${FAILED}" == "Yes"; then
  echo "Fix these errors, and then try again."
else
  echo "  HaLVM Memory Model: ${MEMMODEL_STR}"
  echo
  AC_CONFIG_FILES([mk/autoconf.mk mk/build.mk
                   static-bits/bin/halvm-cabal
                   static-bits/bin/halvm-config
                   static-bits/bin/halvm-ghc
                   static-bits/bin/halvm-ghc-pkg
                  ])
  AC_OUTPUT

  chmod +x static-bits/bin/halvm-cabal
  chmod +x static-bits/bin/halvm-config
  chmod +x static-bits/bin/halvm-ghc
  chmod +x static-bits/bin/halvm-ghc-pkg
fi

dnl ------------------------------------------------------------------------
dnl
dnl Legacy bits.
dnl
dnl ------------------------------------------------------------------------

dnl dnl Figure out if we have a sufficiently recent version of glibc
dnl AC_LANG_PUSH(C)
dnl AC_CACHE_CHECK([whether glibc version >= 2.4], [fp_cv_glibc_better_then_24],
dnl   [AC_RUN_IFELSE([AC_LANG_SOURCE([[
dnl #include <features.h>
dnl 
dnl int main(int a, char *b) {
dnl   return !((__GLIBC__ >= 2) && (__GLIBC_MINOR__ >= 4));
dnl }
dnl ]])],
dnl   [fp_cv_glibc_better_than_24=yes
dnl    echo -n "yes"],
dnl   [fp_cv_glibc_better_than_24=no
dnl    echo -n "no"]]))
dnl AC_LANG_POP(C)
dnl 
dnl if test "$fp_cv_glibc_better_than_24" = "no"; then
dnl   LOG_FAILURE("The HaLVm requires GLIBC >= 2.4.")
dnl fi
dnl 
dnl dnl Make sure we have gmp.so.3
dnl AC_LANG_PUSH(C)
dnl AC_CHECK_LIB(gmp, __gmpz_mul_si, [], LOG_FAILURE("The HaLVM requires GMP (version 3) to be installed."))
dnl AC_LANG_POP(C)
dnl 
dnl dnl See if we have the FUSE library
dnl AC_LANG_PUSH(C)
dnl OLDCPPFLAGS="${CPPFLAGS}"
dnl CPPFLAGS="${CPPFLAGS} -D_FILE_OFFSET_BITS=64"
dnl AC_CHECK_HEADER(fuse.h,[AC_SUBST(HAVE_FUSE_H,yes)],[
dnl   LOG_WARNING("You don't appear to have the FUSE library. This isn't a")
dnl   LOG_WARNINGC(["problem per se, but will make it much more difficult to"])
dnl   LOG_WARNINGC("develop domains that have a file system. We recommend you")
dnl   LOG_WARNINGC("add the FUSE library - usually fuse-devel or similar - before")
dnl   LOG_WARNINGC("continuing.")
dnl   AC_SUBST(HAVE_FUSE_H,no)])
dnl CPPFLAGS="${OLDCPPFLAGS}"
dnl AC_LANG_POP(C)

dnl AC_ARG_ENABLE(bytecode-interpreter,
dnl  [AS_HELP_STRING([--enable-bytecode-interpreter],
dnl    [include support for the GHC runtime to execute bytecode])],
dnl  [AC_SUBST(BYTECODE_INTERP_OPT, "-optc-DALLOW_INTERPRETER")],
dnl  [AC_SUBST(BYTECODE_INTERP_OPT, "")])
dnl 
dnl AC_ARG_ENABLE(sanity-checker,
dnl  [AS_HELP_STRING([--enable-sanity-checker],
dnl    [include support for running the GHC sanity checker outside debug mode])],
dnl  [AC_SUBST(SANITY_CHECKER_OPT, "-optc-DPERFORM_SANITY_CHECKS")],
dnl  [AC_SUBST(SANITY_CHECKER_OPT, "")])
dnl 
dnl ghc_download_name=ghc-6.12.3-${ARCH}-unknown-linux-n.tar.bz2
dnl AC_SUBST(GHC_BINARY_NAME,$ghc_download_name)
dnl 

