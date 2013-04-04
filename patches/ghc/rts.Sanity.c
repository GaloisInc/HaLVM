*** ghc-pristine/rts/Sanity.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Sanity.c	2010-07-02 14:00:43.317498928 -0700
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
