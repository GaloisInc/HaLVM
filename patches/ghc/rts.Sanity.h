*** ghc-pristine/rts/Sanity.h	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Sanity.h	2010-07-02 14:00:43.320499129 -0700
***************
*** 9,15 ****
  #ifndef SANITY_H
  #define SANITY_H
  
! #ifdef DEBUG
  
  BEGIN_RTS_PRIVATE
  
--- 9,15 ----
  #ifndef SANITY_H
  #define SANITY_H
  
! #if defined(DEBUG) || defined(PERFORM_SANITY_CHECKS)
  
  BEGIN_RTS_PRIVATE
  
