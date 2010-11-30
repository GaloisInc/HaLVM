*** ghc-pristine/includes/HaskellConstants.hs	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/HaskellConstants.hs	2010-07-14 16:38:36.934560187 -0700
***************
*** 14,20 ****
  wrong platform settings for the compiler (it has the platform
  settings for the target plat instead).
  -}
! #include "../includes/ghcautoconf.h"
  
  #include "stg/MachRegs.h"
  #include "rts/Constants.h"
--- 14,20 ----
  wrong platform settings for the compiler (it has the platform
  settings for the target plat instead).
  -}
! #include "ghcautoconf.h"
  
  #include "stg/MachRegs.h"
  #include "rts/Constants.h"
