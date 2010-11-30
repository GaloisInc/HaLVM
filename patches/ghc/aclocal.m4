*** ghc-pristine/aclocal.m4	2010-06-09 11:10:08.000000000 -0700
--- xen-ghc/aclocal.m4	2010-07-01 17:06:07.199916215 -0700
***************
*** 236,252 ****
  [
  AC_PATH_PROG(GreenCardCmd,greencard)
  AC_CACHE_CHECK([for version of greencard], fptools_cv_greencard_version,
  changequote(, )dnl
! [if test x"$GreenCardCmd" != x; then
!    fptools_cv_greencard_version="`$GreenCardCmd --version |
! 			  grep 'version' | sed -e 's/greencard. version \([^ ]*\).*/\1/g'`"
  else
     fptools_cv_greencard_version=""
  fi
  changequote([, ])dnl
  ])
! FP_COMPARE_VERSIONS([$fptools_cv_greencard_version],[-lt],[$1],
!   [AC_MSG_ERROR([greencard version $1 or later is required (found '$fptools_cv_greencard_version')])])[]dnl
  GreenCardVersion=$fptools_cv_greencard_version
  AC_SUBST(GreenCardVersion)
  ])
--- 236,251 ----
  [
  AC_PATH_PROG(GreenCardCmd,greencard)
  AC_CACHE_CHECK([for version of greencard], fptools_cv_greencard_version,
+ [
  changequote(, )dnl
! if test x"$GreenCardCmd" != x; then
!    fptools_cv_greencard_version=`$GreenCardCmd --version | grep 'version' | sed -e 's/greencard. version \([^ ]*\).*/\1/g'`
  else
     fptools_cv_greencard_version=""
  fi
  changequote([, ])dnl
  ])
! FP_COMPARE_VERSIONS([$fptools_cv_greencard_version],[-lt],[$1], [AC_MSG_ERROR([greencard version $1 or later is required (found '$fptools_cv_greencard_version')])])[]
  GreenCardVersion=$fptools_cv_greencard_version
  AC_SUBST(GreenCardVersion)
  ])
***************
*** 1344,1350 ****
      $2="linux"
      ;;
    # As far as I'm aware, none of these have relevant variants
!   freebsd|netbsd|openbsd|dragonfly|osf1|osf3|hpux|linuxaout|kfreebsdgnu|freebsd2|solaris2|cygwin32|mingw32|darwin|gnu|nextstep2|nextstep3|sunos4|ultrix|irix|aix|haiku)
      $2="$1"
      ;;
    *)
--- 1343,1349 ----
      $2="linux"
      ;;
    # As far as I'm aware, none of these have relevant variants
!   freebsd|netbsd|openbsd|dragonfly|osf1|osf3|hpux|linuxaout|kfreebsdgnu|freebsd2|solaris2|cygwin32|mingw32|darwin|gnu|nextstep2|nextstep3|sunos4|ultrix|irix|aix|haiku|xen)
      $2="$1"
      ;;
    *)
