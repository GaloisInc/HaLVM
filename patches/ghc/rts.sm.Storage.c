*** ghc-pristine/rts/sm/Storage.c	2010-06-09 11:10:14.000000000 -0700
--- xen-ghc/rts/sm/Storage.c	2010-07-02 14:00:43.338498377 -0700
***************
*** 31,37 ****
--- 31,39 ----
  
  #include <string.h>
  
+ #ifndef xen_HOST_OS
  #include "ffi.h"
+ #endif
  
  /* 
   * All these globals require sm_mutex to access in THREADED_RTS mode.
