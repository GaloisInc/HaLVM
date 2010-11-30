*** ghc-pristine/rts/FrontPanel.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/FrontPanel.c	2010-07-02 13:41:27.127229999 -0700
***************
*** 619,625 ****
--- 619,627 ----
  		    switch (info->type) {
  
  		    case CONSTR:
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  		    case BCO:
+ #endif
  			if (((StgClosure *)p)->header.info == &stg_DEAD_WEAK_info) {
  			    size = sizeofW(StgWeak);
  			    type = Other;
