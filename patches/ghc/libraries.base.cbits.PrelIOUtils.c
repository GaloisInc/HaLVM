*** ghc-pristine/libraries/base/cbits/PrelIOUtils.c	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/cbits/PrelIOUtils.c	2010-07-15 15:01:52.195190831 -0700
***************
*** 25,31 ****
  }
  
  // Use a C wrapper for this because we avoid hsc2hs in base
! #if HAVE_LANGINFO_H
  #include <langinfo.h>
  char *localeEncoding (void)
  {
--- 25,31 ----
  }
  
  // Use a C wrapper for this because we avoid hsc2hs in base
! #if HAVE_LANGINFO_H && !defined(xen_HOST_OS)
  #include <langinfo.h>
  char *localeEncoding (void)
  {
