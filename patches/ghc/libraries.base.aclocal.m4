*** ghc-6.12.3/libraries/base/aclocal.m4	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/aclocal.m4	2012-02-24 10:59:27.953147758 -0800
***************
*** 179,185 ****
  # prototype text as its second argument. It also calls AC_LANG_PROGRAM
  # instead of AC_LANG_CALL
  AC_DEFUN([FP_SEARCH_LIBS_PROTO],
! [AS_VAR_PUSHDEF([ac_Search], [ac_cv_search_$3])dnl
  AC_CACHE_CHECK([for library containing $1], [ac_Search],
  [ac_func_search_save_LIBS=$LIBS
  AC_LANG_CONFTEST([AC_LANG_PROGRAM([$2], [$3])])
--- 179,185 ----
  # prototype text as its second argument. It also calls AC_LANG_PROGRAM
  # instead of AC_LANG_CALL
  AC_DEFUN([FP_SEARCH_LIBS_PROTO],
! [AS_VAR_PUSHDEF([ac_Search], [ac_cv_search_$1])dnl
  AC_CACHE_CHECK([for library containing $1], [ac_Search],
  [ac_func_search_save_LIBS=$LIBS
  AC_LANG_CONFTEST([AC_LANG_PROGRAM([$2], [$3])])
