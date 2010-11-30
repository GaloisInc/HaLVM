*** ghc-pristine/rts/Updates.h	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Updates.h	2010-07-02 14:00:43.346499249 -0700
***************
*** 165,171 ****
  
  #endif /* CMINUSMINUS */
  
! #if !defined(DEBUG) || defined(THREADED_RTS)
  #define DEBUG_FILL_SLOP(p) /* do nothing */
  #else
  #define DEBUG_FILL_SLOP(p) FILL_SLOP(p)
--- 165,171 ----
  
  #endif /* CMINUSMINUS */
  
! #if !defined(DEBUG) || defined(THREADED_RTS) || !defined(PERFORM_SANITY_CHECKS)
  #define DEBUG_FILL_SLOP(p) /* do nothing */
  #else
  #define DEBUG_FILL_SLOP(p) FILL_SLOP(p)
