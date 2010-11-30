*** ghc-pristine/rts/Sanity.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Sanity.c	2010-07-02 14:00:43.317498928 -0700
***************
*** 144,149 ****
--- 144,150 ----
  			 BITMAP_BITS(info->i.layout.bitmap), size);
  	return 1 + size;
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
      case RET_BCO: {
  	StgBCO *bco;
  	nat size;
***************
*** 152,157 ****
--- 153,159 ----
  	checkLargeBitmap((StgPtr)c + 2, BCO_BITMAP(bco), size);
  	return 2 + size;
      }
+ #endif
  
      case RET_BIG: // large bitmap (> 32 entries)
  	size = GET_LARGE_BITMAP(&info->i)->size;
***************
*** 661,668 ****
        break;
  
      default:
!       barf("checkStaticObjetcs: strange closure %p (%s)", 
! 	   p, info_type(p));
      }
    }
  }
--- 663,675 ----
        break;
  
      default:
!       barf("checkStaticObjetcs: strange closure %p (%s)", p,
! #ifndef xen_HOST_OS
!           info_type(p)
! #else    
!           "[HaLVM has no info_type()]"
! #endif
!           );
      }
    }
  }
