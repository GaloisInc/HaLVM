*** ghc-pristine/rts/RtsSignals.h	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/RtsSignals.h	2010-07-02 14:11:41.265438502 -0700
***************
*** 9,15 ****
  #ifndef RTSSIGNALS_H
  #define RTSSIGNALS_H
  
! #if !defined(mingw32_HOST_OS)
  
  #include "posix/Signals.h"
  
--- 9,15 ----
  #ifndef RTSSIGNALS_H
  #define RTSSIGNALS_H
  
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
  
  #include "posix/Signals.h"
  
***************
*** 17,22 ****
--- 17,27 ----
  
  #include "win32/ConsoleHandler.h"
  
+ #elif defined(xen_HOST_OS)
+ 
+ extern int signals_pending(void);
+ extern void startSignalHandlers(Capability *cap);
+ 
  #else
  
  #define signals_pending() (rtsFalse)
