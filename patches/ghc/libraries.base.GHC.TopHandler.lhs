*** ghc-pristine/libraries/base/GHC/TopHandler.lhs	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/GHC/TopHandler.lhs	2010-07-14 19:45:25.487540015 -0700
***************
*** 77,82 ****
--- 77,84 ----
             Close    -> handler
             _ -> return ()
    return ()
+ #elif defined(xen_HOST_OS)
+ install_interrupt_handler _ = return ()
  #else
  #include "rts/Signals.h"
  -- specialised version of System.Posix.Signals.installHandler, which
***************
*** 178,185 ****
--- 180,191 ----
  -- an infinite loop).
  cleanUp :: IO ()
  cleanUp = do
+ #ifndef xen_HOST_OS
    hFlush stdout `catchAny` \_ -> return ()
    hFlush stderr `catchAny` \_ -> return ()
+ #else
+   return ()
+ #endif
  
  -- we have to use unsafeCoerce# to get the 'IO a' result type, since the
  -- compiler doesn't let us declare that as the result type of a foreign export.
***************
*** 188,194 ****
  
  exitInterrupted :: IO a
  exitInterrupted = 
! #ifdef mingw32_HOST_OS
    safeExit 252
  #else
    -- we must exit via the default action for SIGINT, so that the
--- 194,200 ----
  
  exitInterrupted :: IO a
  exitInterrupted = 
! #if defined(mingw32_HOST_OS) || defined(xen_HOST_OS)
    safeExit 252
  #else
    -- we must exit via the default action for SIGINT, so that the
