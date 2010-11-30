*** ghc-pristine/compiler/HsVersions.h	2010-06-09 11:10:08.000000000 -0700
--- xen-ghc/compiler/HsVersions.h	2010-07-14 17:49:15.455581562 -0700
***************
*** 25,31 ****
   * ghcconfig.h, because that will include ghcplatform.h which has the
   * wrong platform settings for the compiler (it has the platform
   * settings for the target plat instead). */
! #include "../includes/ghcautoconf.h"
  
  /* Global variables may not work in other Haskell implementations,
   * but we need them currently! so the conditional on GLASGOW won't do. */
--- 25,31 ----
   * ghcconfig.h, because that will include ghcplatform.h which has the
   * wrong platform settings for the compiler (it has the platform
   * settings for the target plat instead). */
! #include "ghcautoconf.h"
  
  /* Global variables may not work in other Haskell implementations,
   * but we need them currently! so the conditional on GLASGOW won't do. */
