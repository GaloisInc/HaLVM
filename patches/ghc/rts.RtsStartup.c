*** ghc-pristine/rts/RtsStartup.c	2010-06-09 11:10:13.000000000 -0700
--- xen-ghc/rts/RtsStartup.c	2010-07-19 12:38:57.865505045 -0700
***************
*** 108,114 ****
--- 108,116 ----
  	return;
      }
  
+ #ifndef xen_HOST_OS
      setlocale(LC_CTYPE,"");
+ #endif
  
      /* Initialise the stats department, phase 0 */
      initStats0();
***************
*** 180,186 ****
  
      getStablePtr((StgPtr)runSparks_closure);
      getStablePtr((StgPtr)ensureIOManagerIsRunning_closure);
! #ifndef mingw32_HOST_OS
      getStablePtr((StgPtr)runHandlers_closure);
  #endif
  
--- 182,188 ----
  
      getStablePtr((StgPtr)runSparks_closure);
      getStablePtr((StgPtr)ensureIOManagerIsRunning_closure);
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
      getStablePtr((StgPtr)runHandlers_closure);
  #endif
  
***************
*** 188,194 ****
      initGlobalStore();
  
      /* initialise file locking, if necessary */
! #if !defined(mingw32_HOST_OS)    
      initFileLocking();
  #endif
  
--- 190,196 ----
      initGlobalStore();
  
      /* initialise file locking, if necessary */
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
      initFileLocking();
  #endif
  
***************
*** 298,304 ****
--- 300,308 ----
  
      freeGroup_lock(bd);
  
+ #ifndef xen_HOST_OS
      startupHpc();
+ #endif
  
      // This must be done after module initialisation.
      // ToDo: make this work in the presence of multiple hs_add_root()s.
***************
*** 368,374 ****
      exitTimer(wait_foreign);
  
      // set the terminal settings back to what they were
! #if !defined(mingw32_HOST_OS)    
      resetTerminalSettings();
  #endif
  
--- 372,378 ----
      exitTimer(wait_foreign);
  
      // set the terminal settings back to what they were
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
      resetTerminalSettings();
  #endif
  
***************
*** 377,385 ****
  
      /* stop timing the shutdown, we're about to print stats */
      stat_endExit();
!     
      /* shutdown the hpc support (if needed) */
      exitHpc();
  
      // clean up things from the storage manager's point of view.
      // also outputs the stats (+RTS -s) info.
--- 381,391 ----
  
      /* stop timing the shutdown, we're about to print stats */
      stat_endExit();
!    
! #ifndef xen_HOST_OS 
      /* shutdown the hpc support (if needed) */
      exitHpc();
+ #endif
  
      // clean up things from the storage manager's point of view.
      // also outputs the stats (+RTS -s) info.
***************
*** 392,398 ****
      exitGlobalStore();
  
      /* free file locking tables, if necessary */
! #if !defined(mingw32_HOST_OS)    
      freeFileLocking();
  #endif
  
--- 398,404 ----
      exitGlobalStore();
  
      /* free file locking tables, if necessary */
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
      freeFileLocking();
  #endif
  
***************
*** 476,482 ****
      }
  }
  
! #ifndef mingw32_HOST_OS
  void
  shutdownHaskellAndSignal(int sig)
  {
--- 482,488 ----
      }
  }
  
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
  void
  shutdownHaskellAndSignal(int sig)
  {
