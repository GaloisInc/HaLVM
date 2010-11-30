*** ghc-pristine/includes/Rts.h	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/Rts.h	2010-07-02 14:14:14.203687793 -0700
***************
*** 199,205 ****
--- 199,207 ----
  #include "rts/Hooks.h"
  #include "rts/Signals.h"
  #include "rts/BlockSignals.h"
+ #ifndef xen_HOST_OS
  #include "rts/Hpc.h"
+ #endif
  #include "rts/Flags.h"
  #include "rts/Adjustor.h"
  #include "rts/FileLock.h"
