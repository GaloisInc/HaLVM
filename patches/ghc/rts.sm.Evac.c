*** ghc-pristine/rts/sm/Evac.c	2010-06-09 11:10:14.000000000 -0700
--- xen-ghc/rts/sm/Evac.c	2010-07-02 14:00:43.331498398 -0700
***************
*** 637,645 ****
--- 637,647 ----
        copy_tag(p,info,q,sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)),stp,tag);
        return;
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
    case BCO:
        copy(p,info,q,bco_sizeW((StgBCO *)q),stp);
        return;
+ #endif
  
    case CAF_BLACKHOLE:
    case BLACKHOLE:
***************
*** 657,663 ****
--- 659,667 ----
      *p = q;
      goto loop;
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
    case RET_BCO:
+ #endif
    case RET_SMALL:
    case RET_BIG:
    case RET_DYN:
