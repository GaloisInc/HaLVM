*** ghc-pristine/rts/RtsFlags.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/RtsFlags.c	2010-07-02 14:00:43.308379337 -0700
***************
*** 86,91 ****
--- 86,92 ----
      RtsFlags.GcFlags.frontpanel         = rtsFalse;
  #endif
      RtsFlags.GcFlags.idleGCDelayTime    = 300; /* millisecs */
+     RtsFlags.GcFlags.nonDebugSanityChecks = rtsFalse;
  
  #if osf3_HOST_OS
  /* ToDo: Perhaps by adjusting this value we can make linking without
