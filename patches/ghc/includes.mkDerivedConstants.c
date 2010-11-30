*** ghc-pristine/includes/mkDerivedConstants.c	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/mkDerivedConstants.c	2010-07-01 16:40:32.430379009 -0700
***************
*** 369,374 ****
--- 369,375 ----
      closure_field(StgMVar,tail);
      closure_field(StgMVar,value);
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      closure_size(StgBCO);
      closure_field(StgBCO, instrs);
      closure_field(StgBCO, literals);
***************
*** 376,381 ****
--- 377,383 ----
      closure_field(StgBCO, arity);
      closure_field(StgBCO, size);
      closure_payload(StgBCO, bitmap);
+ #endif // (xen_HOST_OS && ALLOW_INTERPRETER) || !xen_HOST_OS
  
      closure_size(StgStableName);
      closure_field(StgStableName,sn);
