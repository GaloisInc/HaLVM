*** ghc-pristine/rts/sm/Compact.c	2010-06-09 11:10:14.000000000 -0700
--- xen-ghc/rts/sm/Compact.c	2010-07-02 14:00:43.328499120 -0700
***************
*** 353,358 ****
--- 353,359 ----
  	    }
  	    continue;
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  	case RET_BCO: {
  	    StgBCO *bco;
  	    nat size;
***************
*** 366,371 ****
--- 367,373 ----
  	    p += size;
  	    continue;
  	}
+ #endif
  
  	    // large bitmap (> 32 entries, or 64 on a 64-bit machine) 
  	case RET_BIG:
***************
*** 416,425 ****
--- 418,429 ----
  	thread_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
  	p += size;
  	break;
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case ARG_BCO:
  	thread_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
  	p += size;
  	break;
+ #endif
      default:
  	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
      small_bitmap:
***************
*** 599,605 ****
  	thread(&((StgClosure *)p)->payload[0]);
  	thread(&((StgClosure *)p)->payload[1]);
  	return p + sizeofW(StgHeader) + 2;
! 	
      case BCO: {
  	StgBCO *bco = (StgBCO *)p;
  	thread_(&bco->instrs);
--- 603,610 ----
  	thread(&((StgClosure *)p)->payload[0]);
  	thread(&((StgClosure *)p)->payload[1]);
  	return p + sizeofW(StgHeader) + 2;
! 
! #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case BCO: {
  	StgBCO *bco = (StgBCO *)p;
  	thread_(&bco->instrs);
***************
*** 607,612 ****
--- 612,618 ----
  	thread_(&bco->ptrs);
  	return p + bco_sizeW(bco);
      }
+ #endif
  
      case THUNK:
      {
