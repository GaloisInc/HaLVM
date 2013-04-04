*** ghc-pristine/rts/Schedule.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/Schedule.c	2010-07-02 14:00:43.325498422 -0700
***************
*** 117,123 ****
  Mutex sched_mutex;
  #endif
  
! #if !defined(mingw32_HOST_OS)
  #define FORKPROCESS_PRIMOP_SUPPORTED
  #endif
  
--- 117,123 ----
  Mutex sched_mutex;
  #endif
  
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
  #define FORKPROCESS_PRIMOP_SUPPORTED
  #endif
  
***************
*** 1003,1008 ****
--- 1005,1020 ----
  static void
  scheduleSendPendingMessages(void)
  {
+ #ifdef xen_HOST_OS
+   if( emptyThreadQueues(cap) ) {
+     if( RtsFlags.MiscFlags.install_signal_handlers && anyUserHandlers() ) {
+       awaitUserSignals();
+       if(signals_pending()) {
+         startSignalHandlers(cap);
+       }
+     }
+   }
+ #else 
  
  # if defined(PAR) // global Mem.Mgmt., omit for now
      if (PendingFetches != END_BF_QUEUE) {
***************
*** 1031,1036 ****
--- 1043,1050 ----
          createSparkThread(cap);
          debugTrace(DEBUG_sched, "creating a spark thread");
      }
+ 
+ #endif // ifndef xen_HOST_OS
  }
  #endif // PARALLEL_HASKELL || THREADED_RTS
  
