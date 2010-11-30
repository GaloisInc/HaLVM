*** ghc-pristine/rts/ProfHeap.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/ProfHeap.c	2010-07-02 14:00:43.305375129 -0700
***************
*** 774,780 ****
  
  	if (count == 0) continue;
  
! #if !defined(PROFILING)
  	switch (RtsFlags.ProfFlags.doHeapProfile) {
  	case HEAP_BY_CLOSURE_TYPE:
  	    fprintf(hp_file, "%s", (char *)ctr->identity);
--- 774,780 ----
  
  	if (count == 0) continue;
  
! #if defined(PROFILING)
  	switch (RtsFlags.ProfFlags.doHeapProfile) {
  	case HEAP_BY_CLOSURE_TYPE:
  	    fprintf(hp_file, "%s", (char *)ctr->identity);
***************
*** 904,913 ****
--- 904,915 ----
  		size = BLACKHOLE_sizeW();
  		break;
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  	    case BCO:
  		prim = rtsTrue;
  		size = bco_sizeW((StgBCO *)p);
  		break;
+ #endif
  
              case MVAR_CLEAN:
              case MVAR_DIRTY:
