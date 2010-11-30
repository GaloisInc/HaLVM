*** ghc-pristine/rts/Interpreter.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Interpreter.c	2010-07-02 13:41:27.133623459 -0700
***************
*** 27,32 ****
--- 27,33 ----
  #include <errno.h>
  #endif
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  #include "ffi.h"
  
  /* --------------------------------------------------------------------------
***************
*** 1493,1495 ****
--- 1494,1498 ----
  
      barf("interpretBCO: fell off end of the interpreter");
  }
+ 
+ #endif /* (xen_HOST_OS && ALLOW_INTERPRETER) || !xen_HOST_OS */
