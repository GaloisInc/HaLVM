AC_PREREQ(2.60)
AC_INIT(HaLVM,3.0.0,awick@galois.com,halvm)

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

AC_FORCE_EXIST(ALEX,alex)
AC_FORCE_EXIST(AR,ar)
AC_FORCE_EXIST(GCC,gcc)
AC_FORCE_EXIST(GHC,ghc)
AC_FORCE_EXIST(LD,ld)
AC_FORCE_EXIST(LN,ln)
AC_FORCE_EXIST(INSTALL, install)
AC_FORCE_EXIST(NM,nm)
AC_FORCE_EXIST(MAKE,make)
AC_FORCE_EXIST(OBJDUMP,objdump)
AC_FORCE_EXIST(RANLIB,ranlib)
AC_FORCE_EXIST(READELF,readelf)
AC_FORCE_EXIST(SED,sed)

AC_CHECK_PROG_VER(ALEX, alex, --version, [Alex version \([0-9.]*\)],
                  [3.*], LOG_FAILURE("Couldn't find required version of Alex"))
AC_CHECK_PROG_VER(CABAL, cabal, --version, [cabal-install version \([0-9.]*\)],
                  [1.2[456789].*],
                  LOG_FAILURE("Couldn't find required version of Cabal"))
AC_CHECK_PROG_VER(GHC, ghc, --version, [version \([0-9.]*\)],
                 [8.0.*], LOG_FAILURE("Couldn't find required version of GHC"))
AC_CHECK_PROG_VER(HADDOCK, haddock, --version, [Haddock version \([0-9.]*\)],
                  [2.17.*],
                  LOG_FAILURE("Couldn't find required version of Haddock"))
AC_CHECK_PROG_VER(HAPPY, happy, --version, [Happy Version \([0-9.]*\)],
                  [1.19.*],
                  LOG_FAILURE("Couldn't find required version of Happy"))
AC_CHECK_PROG_VER(HSCOLOUR, HsColour, --version, [HsColour \([0-9.]*\)],
                  [1.24*],
                  LOG_FAILURE("Couldn't find required version of HsColour"))

AC_PROG_CC
AC_PROG_CXX
AC_PROG_CXXCPP

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
              AC_SUBST(PLATFORM,"centos67"),
              AC_SUBST(PLATFORM,"deb8"))

architecture=`uname -p`
if test "$architecture" == "x86_64"; then
  AC_SUBST(ARCH,"x86_64")
else
  AC_SUBST(ARCH,"i386")
fi

AC_SUBST(target,"${ARCH}-unknown-HaLVM")
AC_SUBST(target_prefix,"${ARCH}-unknown-HaLVM-")


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
  echo "  Integer library: ${INTEGER_LIBRARY}"
  echo
  AC_CONFIG_FILES([
                   autoconf.mk
                   misc/build.mk
                   examples/standard.mk
                   scripts/halvm-ghc
                  ])
  AC_OUTPUT
fi

