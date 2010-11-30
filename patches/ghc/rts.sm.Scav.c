*** ghc-pristine/rts/sm/Scav.c	2010-06-09 11:10:14.000000000 -0700
--- xen-ghc/rts/sm/Scav.c	2010-07-02 14:00:43.335503572 -0700
***************
*** 243,252 ****
--- 243,254 ----
  	scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
  	p += size;
  	break;
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case ARG_BCO:
  	scavenge_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
  	p += size;
  	break;
+ #endif
      default:
  	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
      small_bitmap:
***************
*** 543,548 ****
--- 545,551 ----
  	break;
      }
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case BCO: {
  	StgBCO *bco = (StgBCO *)p;
  	evacuate((StgClosure **)&bco->instrs);
***************
*** 551,556 ****
--- 554,560 ----
  	p += bco_sizeW(bco);
  	break;
      }
+ #endif
  
      case IND_PERM:
        if (bd->gen_no != 0) {
***************
*** 918,923 ****
--- 922,928 ----
  	    break;
  	}
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  	case BCO: {
  	    StgBCO *bco = (StgBCO *)p;
  	    evacuate((StgClosure **)&bco->instrs);
***************
*** 925,930 ****
--- 930,936 ----
  	    evacuate((StgClosure **)&bco->ptrs);
  	    break;
  	}
+ #endif
  
  	case IND_PERM:
  	    // don't need to do anything here: the only possible case
***************
*** 1812,1817 ****
--- 1818,1824 ----
  	    scavenge_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap);
  	continue;
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case RET_BCO: {
  	StgBCO *bco;
  	nat size;
***************
*** 1825,1830 ****
--- 1832,1838 ----
  	p += size;
  	continue;
      }
+ #endif
  
        // large bitmap (> 32 entries, or > 64 on a 64-bit machine) 
      case RET_BIG:
