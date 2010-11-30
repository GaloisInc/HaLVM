*** ghc-pristine/includes/rts/storage/ClosureMacros.h	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/rts/storage/ClosureMacros.h	2010-07-02 11:00:49.838447808 -0700
***************
*** 283,290 ****
--- 283,292 ----
  INLINE_HEADER StgWord tso_sizeW ( StgTSO *tso )
  { return TSO_STRUCT_SIZEW + tso->stack_size; }
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  INLINE_HEADER StgWord bco_sizeW ( StgBCO *bco )
  { return bco->size; }
+ #endif
  
  INLINE_HEADER nat
  closure_sizeW_ (StgClosure *p, StgInfoTable *info)
***************
*** 333,340 ****
--- 335,344 ----
  	return mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
      case TSO:
  	return tso_sizeW((StgTSO *)p);
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case BCO:
  	return bco_sizeW((StgBCO *)p);
+ #endif
      case TVAR_WATCH_QUEUE:
          return sizeofW(StgTVarWatchQueue);
      case TVAR:
***************
*** 384,391 ****
--- 388,397 ----
      case RET_BIG:
  	return 1 + GET_LARGE_BITMAP(&info->i)->size;
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case RET_BCO:
  	return 2 + BCO_BITMAP_SIZE((StgBCO *)((P_)frame)[1]);
+ #endif
  
      default:
  	return 1 + BITMAP_SIZE(info->i.layout.bitmap);
