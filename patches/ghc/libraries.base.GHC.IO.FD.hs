*** ghc-pristine/libraries/base/GHC/IO/FD.hs	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/GHC/IO/FD.hs	2010-07-19 11:21:09.737489991 -0700
***************
*** 371,379 ****
--- 371,390 ----
  #endif
    return (toEnum (fromIntegral r))
  
+ #ifndef xen_HOST_OS
+ 
  foreign import ccall safe "fdReady"
    fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt
  
+ #else
+ 
+ fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt
+ fdReady  = error "No fdReady on HaLVM"
+ 
+ #endif
+ 
+ 
+ 
  -- ---------------------------------------------------------------------------
  -- Terminal-related stuff
  
***************
*** 526,534 ****
--- 537,554 ----
  isNonBlocking :: FD -> Bool
  isNonBlocking fd = fdIsNonBlocking fd /= 0
  
+ #ifndef xen_HOST_OS
+ 
  foreign import ccall unsafe "fdReady"
    unsafe_fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt
  
+ #else
+ 
+ unsafe_fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt
+ unsafe_fdReady  = error "No unsafe_fdReady on HaLVM"
+ 
+ #endif
+ 
  #else /* mingw32_HOST_OS.... */
  
  readRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
***************
*** 618,623 ****
--- 638,645 ----
  -- -----------------------------------------------------------------------------
  -- Locking/unlocking
  
+ #ifndef xen_HOST_OS
+ 
  #ifndef mingw32_HOST_OS
  foreign import ccall unsafe "lockFile"
    lockFile :: CInt -> CDev -> CIno -> CInt -> IO CInt
***************
*** 626,631 ****
--- 648,663 ----
    unlockFile :: CInt -> IO CInt
  #endif
  
+ #else
+ 
+ lockFile :: CInt -> CDev -> CIno -> CInt -> IO CInt
+ lockFile  = error "No lockFile on HaLVM"
+ 
+ unlockFile :: CInt -> IO CInt
+ unlockFile  = error "No unlockFile on HaLVM"
+ 
+ #endif
+ 
  #if defined(DEBUG_DUMP)
  puts :: String -> IO ()
  puts s = do withCStringLen s $ \(p,len) -> c_write 1 p (fromIntegral len)
