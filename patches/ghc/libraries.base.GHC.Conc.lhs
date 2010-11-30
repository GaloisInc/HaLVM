*** ghc-pristine/libraries/base/GHC/Conc.lhs	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/GHC/Conc.lhs	2010-07-14 19:43:16.515250276 -0700
***************
*** 77,88 ****
          , asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
  #endif
  
! #ifndef mingw32_HOST_OS
          , Signal, HandlerFun, setHandler, runHandlers
  #endif
  
          , ensureIOManagerIsRunning
! #ifndef mingw32_HOST_OS
          , syncIOManager
  #endif
  
--- 77,88 ----
          , asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
  #endif
  
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
          , Signal, HandlerFun, setHandler, runHandlers
  #endif
  
          , ensureIOManagerIsRunning
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
          , syncIOManager
  #endif
  
***************
*** 98,104 ****
          ) where
  
  import System.Posix.Types
! #ifndef mingw32_HOST_OS
  import System.Posix.Internals
  #endif
  import Foreign
--- 98,104 ----
          ) where
  
  import System.Posix.Types
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
  import System.Posix.Internals
  #endif
  import Foreign
***************
*** 118,126 ****
--- 118,128 ----
  #ifndef mingw32_HOST_OS
  import GHC.Debug
  #endif
+ #ifdef xen_HOST_OS
  import {-# SOURCE #-} GHC.IO.Handle ( hFlush )
  import {-# SOURCE #-} GHC.IO.Handle.FD ( stdout )
  import GHC.IO
+ #endif
  import GHC.IO.Exception
  import GHC.Exception
  import GHC.IORef
***************
*** 670,675 ****
--- 672,678 ----
  -- -----------------------------------------------------------------------------
  -- Thread IO API
  
+ #ifndef xen_HOST_OS
  -- | Block the current thread until data is available to read on the
  -- given file descriptor (GHC only).
  threadWaitRead :: Fd -> IO ()
***************
*** 693,698 ****
--- 696,702 ----
          case fromIntegral fd of { I# fd# ->
          case waitWrite# fd# s of { s' -> (# s', () #)
          }}
+ #endif
  
  -- | Suspends the current thread for a given number of microseconds
  -- (GHC only).
***************
*** 716,722 ****
--- 720,735 ----
  registerDelay :: Int -> IO (TVar Bool)
  registerDelay usecs 
    | threaded = waitForDelayEventSTM usecs
+ #ifdef xen_HOST_OS
+   | otherwise = do
+       m <- atomically $ newTVar False
+       forkIO $ do
+         threadDelay usecs
+         atomically $ writeTVar m True
+       return m
+ #else
    | otherwise = error "registerDelay: requires -threaded"
+ #endif
  
  foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
  
***************
*** 802,807 ****
--- 815,821 ----
    | threaded  = startIOManagerThread
    | otherwise = return ()
  
+ #ifndef xen_HOST_OS
  startIOManagerThread :: IO ()
  startIOManagerThread = do
    modifyMVar_ ioManagerThread $ \old -> do
***************
*** 814,819 ****
--- 828,834 ----
            ThreadFinished -> create
            ThreadDied     -> create
            _other         -> return (Just t)
+ #endif
  
  insertDelay :: DelayReq -> [DelayReq] -> [DelayReq]
  insertDelay d [] = [d]
***************
*** 830,835 ****
--- 845,851 ----
  foreign import ccall unsafe "getUSecOfDay" 
    getUSecOfDay :: IO USecs
  
+ #ifndef xen_HOST_OS
  {-# NOINLINE prodding #-}
  prodding :: IORef Bool
  prodding = unsafePerformIO $ do
***************
*** 846,851 ****
--- 862,868 ----
    -- blocked in select().
    was_set <- atomicModifyIORef prodding $ \b -> (True,b)
    if (not (was_set)) then  wakeupIOManager else return ()
+ #endif
  
  -- Machinery needed to ensure that we only have one copy of certain
  -- CAFs in this module even when the base package is present twice, as
***************
*** 988,994 ****
--- 1005,1031 ----
  foreign import stdcall "WaitForSingleObject"
     c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD
  
+ #elif defined(xen_HOST_OS)
+ 
+ -- Xen IO Manager thread
+ 
+ prodServiceThread :: IO ()
+ prodServiceThread  = return ()
+ 
+ startIOManagerThread :: IO ()
+ startIOManagerThread  = return ()
+ 
+ signalHandlerLock :: MVar ()
+ signalHandlerLock  = unsafePerformIO (newMVar ())
+ 
+ threadWaitRead :: Fd -> IO ()
+ threadWaitRead _ = fail "threadWaitRead is not proper on the HaLVM"
+ 
+ threadWaitWrite :: Fd -> IO ()
+ threadWaitWrite  _ = fail "threadWwaitWrite is not proper on the HaLVM"
+ 
  #else
+ 
  -- ----------------------------------------------------------------------------
  -- Unix IO manager thread, using select()
  
