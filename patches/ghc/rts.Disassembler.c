*** ghc-pristine/rts/Disassembler.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Disassembler.c	2010-07-02 13:41:27.124623396 -0700
***************
*** 9,14 ****
--- 9,15 ----
   * ---------------------------------------------------------------------------*/
  
  #ifdef DEBUG
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  
  #include "PosixSource.h"
  #include "Rts.h"
***************
*** 287,290 ****
--- 288,292 ----
     ASSERT(pc == nbcs+1);
  }
  
+ #endif /* (xen_HOST_OS && ALLOW_INTERPRETER) || !xen_HOST_OS */
  #endif /* DEBUG */
