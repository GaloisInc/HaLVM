*** ghc-pristine/configure.ac	2010-06-09 11:10:11.000000000 -0700
--- xen-ghc/configure.ac	2010-07-01 17:02:56.772725041 -0700
***************
*** 79,85 ****
  
  dnl ** What command to use to compile compiler sources ?
  dnl --------------------------------------------------------------
- 
  AC_ARG_WITH([ghc],
  [AC_HELP_STRING([--with-ghc=ARG],
    [Use ARG as the path to GHC  [default=autodetect]])],
--- 79,84 ----
***************
*** 351,357 ****
  
  checkOS() {
      case $1 in
!     linux|freebsd|netbsd|openbsd|dragonfly|osf1|osf3|hpux|linuxaout|kfreebsdgnu|freebsd2|solaris2|cygwin32|mingw32|darwin|gnu|nextstep2|nextstep3|sunos4|ultrix|irix|aix|haiku)
          ;;
      *)
          echo "Unknown OS '$1'"
--- 350,356 ----
  
  checkOS() {
      case $1 in
!     linux|freebsd|netbsd|openbsd|dragonfly|osf1|osf3|hpux|linuxaout|kfreebsdgnu|freebsd2|solaris2|cygwin32|mingw32|darwin|gnu|nextstep2|nextstep3|sunos4|ultrix|irix|aix|haiku|xen)
          ;;
      *)
          echo "Unknown OS '$1'"
***************
*** 526,532 ****
  if test x"$TargetPlatform" = x"i386-unknown-mingw32"; then
     GhcLibsWithUnix=NO
  else
!    GhcLibsWithUnix=YES
  fi
  AC_SUBST([GhcLibsWithUnix])
  
--- 525,535 ----
  if test x"$TargetPlatform" = x"i386-unknown-mingw32"; then
     GhcLibsWithUnix=NO
  else
!   if test x"$HostOS_CPP" = x"xen"; then
!     GhcLibsWithUnix=NO
!   else
!     GhcLibsWithUnix=YES
!   fi
  fi
  AC_SUBST([GhcLibsWithUnix])
  
***************
*** 658,664 ****
  AC_SYS_LARGEFILE
  
  dnl ** check for specific header (.h) files that we are interested in
! AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/mman.h sys/resource.h sys/select.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h time.h utime.h windows.h winsock.h sched.h])
  
  dnl ** check if it is safe to include both <time.h> and <sys/time.h>
  AC_HEADER_TIME
--- 661,671 ----
  AC_SYS_LARGEFILE
  
  dnl ** check for specific header (.h) files that we are interested in
! if test x"$HostOS_CPP" = x"xen"; then
!    AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pwd.h sys/mman.h sys/resource.h sys/select.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h time.h utime.h windows.h winsock.h sched.h])
! else
!   AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/mman.h sys/resource.h sys/select.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h time.h utime.h windows.h winsock.h sched.h])
! fi
  
  dnl ** check if it is safe to include both <time.h> and <sys/time.h>
  AC_HEADER_TIME
***************
*** 783,795 ****
  dnl ################################################################
  
  dnl ** check whether we need -ldl to get dlopen()
! 
! AC_CHECK_LIB(dl, dlopen,
!     [HaveLibDL=YES
!      AC_DEFINE([HAVE_LIBDL], [1], [Define to 1 if you need -ldl to get dlopen().])
!      LIBS="$LIBS -ldl"],
!     [HaveLibDL=NO])
! AC_SUBST(HaveLibDL)
  
  dnl --------------------------------------------------
  dnl * Miscellaneous feature tests
--- 790,805 ----
  dnl ################################################################
  
  dnl ** check whether we need -ldl to get dlopen()
! if test "$HostOS_CPP" = "xen"; then
!    HaveLibDL=NO
! else
!    AC_CHECK_LIB(dl, dlopen,
!        [HaveLibDL=YES
!         AC_DEFINE([HAVE_LIBDL], [1], [Define to 1 if you need -ldl to get dlopen().])
!         LIBS="$LIBS -ldl"],
!        [HaveLibDL=NO])
!    AC_SUBST(HaveLibDL)
! fi
  
  dnl --------------------------------------------------
  dnl * Miscellaneous feature tests
***************
*** 847,855 ****
--- 857,867 ----
  CFLAGS="$CFLAGS2"
  
  dnl ** check for librt
+ if test "$HostOS_CPP" != "xen"; then
  AC_CHECK_LIB(rt, clock_gettime)
  AC_CHECK_FUNCS(clock_gettime timer_create timer_settime)
  FP_CHECK_TIMER_CREATE
+ fi
  
  dnl ** check for Apple's "interesting" long double compatibility scheme
  AC_MSG_CHECKING(for printf\$LDBLStub)
