*** ghc-6.10.1/libraries/base/cbits/selectUtils.c	2008-11-03 10:13:59.000000000 -0800
--- xen-ghc/libraries/base/cbits/selectUtils.c	2009-01-13 10:22:07.000000000 -0800
***************
*** 1,3 ****
--- 1,5 ----
  
  #include "HsBase.h"
+ #ifndef xen_HOST_OS
  void hsFD_ZERO(fd_set *fds) { FD_ZERO(fds); }
+ #endif
