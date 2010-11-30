*** ghc-pristine/libraries/array/Data/Array/IO.hs	2009-09-21 08:57:56.000000000 -0700
--- xen-ghc/libraries/array/Data/Array/IO.hs	2010-07-15 16:10:05.260978160 -0700
***************
*** 25,33 ****
--- 25,35 ----
     -- * Overloaded mutable array interface
     module Data.Array.MArray,
  
+ #ifndef xen_HOST_OS
     -- * Doing I\/O with @IOUArray@s
     hGetArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
     hPutArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
+ #endif
   ) where
  
  import Data.Array.Base
***************
*** 42,50 ****
--- 44,54 ----
  import GHC.Exts  (MutableByteArray#, RealWorld)
  import GHC.Arr
  import GHC.IORef
+ #ifndef xen_HOST_OS
  import GHC.IO.Handle
  import GHC.IO.Buffer
  import GHC.IO.Exception
+ #endif
  
  #else
  import Data.Char
***************
*** 52,61 ****
  import System.IO
  #endif
  
  #ifdef __GLASGOW_HASKELL__
  -- ---------------------------------------------------------------------------
  -- hGetArray
- 
  -- | Reads a number of 'Word8's from the specified 'Handle' directly
  -- into an array.
  hGetArray
--- 56,65 ----
  import System.IO
  #endif
  
+ #ifndef xen_HOST_OS
  #ifdef __GLASGOW_HASKELL__
  -- ---------------------------------------------------------------------------
  -- hGetArray
  -- | Reads a number of 'Word8's from the specified 'Handle' directly
  -- into an array.
  hGetArray
***************
*** 150,152 ****
--- 154,157 ----
  illegalBufferSize _ fn sz = ioError $
  	userError (fn ++ ": illegal buffer size " ++ showsPrec 9 (sz::Int) [])
  #endif /* !__GLASGOW_HASKELL__ */
+ #endif
