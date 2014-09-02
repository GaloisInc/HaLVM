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
AC_FORCE_EXIST(AR,ar)
AC_FORCE_EXIST(AUTOMAKE,automake)
AC_FORCE_EXIST(AUTORECONF,autoreconf)
AC_FORCE_EXIST(CABAL,cabal)
AC_FORCE_EXIST(CC,gcc)
AC_FORCE_EXIST(CHMOD,chmod)
AC_FORCE_EXIST(CP,cp)
AC_FORCE_EXIST(CPP,cpp)
AC_FORCE_EXIST(ECHO,echo)
AC_FORCE_EXIST(FIND,find)
AC_FORCE_EXIST(GIT,git)
AC_FORCE_EXIST(INSTALL,install)
AC_FORCE_EXIST(LD,ld)
AC_FORCE_EXIST(LIBTOOL,libtool) dnl Required by Ubuntu 12 for autoreconf
AC_FORCE_EXIST(LN,ln)
AC_FORCE_EXIST(MAKE,make)
AC_FORCE_EXIST(MKDIR,mkdir)
AC_FORCE_EXIST(MV,mv)
AC_FORCE_EXIST(NM,nm)
AC_FORCE_EXIST(OBJDUMP,objdump)
AC_FORCE_EXIST(PATCH,patch)
AC_FORCE_EXIST(PERL,perl)
AC_FORCE_EXIST(RANLIB,ranlib)
AC_FORCE_EXIST(RM,rm)
AC_FORCE_EXIST(SED,sed)
AC_FORCE_EXIST(TAR,tar)
AC_FORCE_EXIST(TOUCH,touch)

AC_CHECK_PROG_VER(GHC, ghc, --version, [version \([0-9.]*\)], [[789].*],
                  LOG_FAILURE("The HaLVM requires GHC 7 or higher."))
AC_CHECK_PROG_VER(ALEX, alex, --version, [version \([0-9.]*\)],
                  [3.[1-9]*],
                  LOG_FAILURE("The HaLVM requires alex 3.1+."))
AC_CHECK_PROG_VER(HAPPY, happy, --version, [Version \([0-9.]*\)], [1.19.*],
                  LOG_FAILURE("The HaLVM requires Happy 1.19+."))

AC_SUBST(FORCE_PLATFORM_GHC,no)
AC_CHECK_PROG_VER(GHC, ghc, --version, [version \([0-9.]*\)], [7.8.*],
                  AC_SUBST(FORCE_PLATFORM_GHC,yes))

AC_CHECK_HEADER(ncurses.h, HAVE_NCURSES=y,
  [AC_CHECK_HEADER(curses.h, HAVE_NCURSES=y,
     LOG_FAILURE("The HaLVM requires the ncurses development library."))])

if test "${FORCE_PLATFORM_GHC}" = "yes"; then
  AC_FORCE_EXIST(CURL,curl)
  AC_SUBST(GHC,"\$(TOPDIR)/platform_ghc/bin/ghc")
else
  AC_MSG_CHECKING(for globally-installed GHC terminfo package)
  if ${GHC}-pkg list --simple-output --no-user-package-conf | grep -q terminfo;
  then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    LOG_FAILURE("The HaLVM requires GHC's terminfo library to be installed globally.")
  fi
fi

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
AC_ARG_ENABLE([gmp],
  [AC_HELP_STRING([--enable-gmp],
                  [Build with gmp instead of integer-simple])],
  [if test "$enableval" = "yes"; then
     int_library=integer-gmp
   else
     int_library=integer-simple
   fi],
  [int_library=integer-simple])
AC_SUBST(INTEGER_LIBRARY, [$int_library])

AC_CHECK_FILE(/etc/centos-release,
              AC_SUBST(PLATFORM,"centos65"),
              AC_SUBST(PLATFORM,"deb7"))

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
  ], [enable_docs="maybe"])

case "$enable_docs" in
  "yes")
           AC_FORCE_EXIST(HADDOCK,haddock)
           AC_FORCE_EXIST(HSCOLOUR,HsColour)
           AC_SUBST(ENABLE_DOCS,"YES")
           ;;
  "no")
           AC_SUBST(ENABLE_DOCS,"NO")
           ;;
  "maybe")
           AC_PATH_PROGS(HADDOCK,haddock,no)
           AC_PATH_PROGS(HSCOLOUR,HsColour,no)
           if test "${HADDOCK}" == "no"; then
             HADDOCK_TEST="no"
           else
             HADDOCK_TEST="yes"
           fi
           if test "${HSCOLOUR}" == "no"; then
             HSCOLOUR_TEST="no"
           else
             HSCOLOUR_TEST="yes"
           fi
           case "${HADDOCK_TEST}${HSCOLOUR_TEST}" in
             "nono")   AC_SUBST(ENABLE_DOCS,"NO")
                       doc_reason="(no haddock or hscolour)";;
             "yesno")  AC_SUBST(ENABLE_DOCS,"NO")
                       doc_reason="(no hscolour)";;
             "noyes")  AC_SUBST(ENABLE_DOCS,"NO")
                       doc_reason="(no haddock)";;
             "yesyes") AC_SUBST(ENABLE_DOCS,"YES");;
             *) echo "Fail: |${HADDOCK}${HSCOLOUR}|"
           esac
           ;;
esac

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
  AC_SUBST(XEN_LIBRARY_DIRECTORY,"${withval}/dist/install/usr/lib")
  AC_SUBST(XEN_BIN_ROOT,"${withval}/dist/install/")],
 [AC_SUBST(XEN_BIN_ROOT,"/")
  AC_SUBST(XEN_INCLUDE_DIRECTORY,"/usr/include")
  AC_SUBST(XEN_LIBRARY_DIRECTORY,"/usr/lib")])

dnl First, see if we have Xen around.
AC_CHECK_HEADER(xen/xen.h,[],[
  LOG_FAILURE("Couldn't find the Xen headers. Please either install them")
  LOG_FAILUREC(["(they're probably in something like xen-devel), or use the"])
  LOG_FAILUREC(["--with-xen-tree configure flag to point this script at the"])
  LOG_FAILUREC(["base of a Xen source repository checkout."])])

AC_CHECK_HEADER(xenstore.h, HAVE_XENSTORE_H=yes, HAVE_XENSTORE_H=no)
AC_CHECK_HEADER(xs.h, HAVE_XS_H=yes, HAVE_XS_H=no)

if test "${HAVE_XENSTORE_H}${HAVE_XS_H}" = "nono"; then
  LOG_FAILURE("The HaLVM requires either xenstore.h or xs.h.")
fi

if test "${HAVE_XENSTORE_H}" = "yes"; then
  AC_SUBST(XENSTORE_H_OPT, "-DHAVE_XENSTORE_H")
else
  AC_SUBST(XENSTORE_H_OPT, "")
fi

dnl Is this a 64-bit platform?
if test `uname -m` = "x86_64"; then
  memory_model="64";
  arch="x86_64"
  abi="64"
else
  arch="i386"
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
  echo
  echo "Fix these errors, and then try again."
else
  echo
  echo "  HaLVM Memory Model: ${MEMMODEL_STR}"
  echo "  Integer library: ${INTEGER_LIBRARY}"
  echo "  Building documentation: ${ENABLE_DOCS} ${doc_reason}"
  echo
  AC_CONFIG_FILES([
                   autoconf.mk
                   examples/standard.mk
                   src/HALVMCore/HALVMCore.buildinfo
                   src/misc/build.mk
                   src/scripts/halvm-cabal
                   src/scripts/halvm-config
                   src/scripts/halvm-ghc
                   src/scripts/halvm-ghc-pkg
                   src/scripts/ldkernel
                  ])
  AC_OUTPUT
fi
