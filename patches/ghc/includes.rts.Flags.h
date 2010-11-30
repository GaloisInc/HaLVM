*** ghc-pristine/includes/rts/Flags.h	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/rts/Flags.h	2010-07-02 11:07:46.954166664 -0700
***************
*** 52,57 ****
--- 52,58 ----
      int idleGCDelayTime;	/* in milliseconds */
  
      StgWord heapBase;           /* address to ask the OS for memory */
+     rtsBool nonDebugSanityChecks;
  };
  
  struct DEBUG_FLAGS {  
