*** ghc-pristine/rts/Timer.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Timer.c	2010-07-02 14:00:43.343498500 -0700
***************
*** 26,32 ****
  #include "RtsSignals.h"
  
  /* ticks left before next pre-emptive context switch */
! static int ticks_to_ctxt_switch = 0;
  
  #if defined(THREADED_RTS)
  /* idle ticks left before we perform a GC */
--- 26,32 ----
  #include "RtsSignals.h"
  
  /* ticks left before next pre-emptive context switch */
!        int ticks_to_ctxt_switch = 0;
  
  #if defined(THREADED_RTS)
  /* idle ticks left before we perform a GC */
