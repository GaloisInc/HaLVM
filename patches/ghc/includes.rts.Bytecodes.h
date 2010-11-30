*** ghc-pristine/includes/rts/Bytecodes.h	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/rts/Bytecodes.h	2010-07-02 10:40:34.506560002 -0700
***************
*** 6,11 ****
--- 6,13 ----
   *
   * ---------------------------------------------------------------------------*/
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
+ 
  /* --------------------------------------------------------------------------
   * Instructions
   *
***************
*** 92,94 ****
--- 94,97 ----
  #define INTERP_STACK_CHECK_THRESH  50
  
  /*-------------------------------------------------------------------------*/
+ #endif // (xen_HOST_OS && ALLOW_INTERPRETER) || !xen_HOST_OS
