*** ghc-pristine/libraries/base/System/IO.hs	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/System/IO.hs	2010-07-15 16:44:07.715527167 -0700
***************
*** 143,148 ****
--- 143,149 ----
      readIO,                    -- :: Read a => String -> IO a
      readLn,                    -- :: Read a => IO a
  
+ 
      -- * Binary input and output
  
      withBinaryFile,
***************
*** 162,167 ****
--- 163,173 ----
      openTempFileWithDefaultPermissions,
      openBinaryTempFileWithDefaultPermissions,
  
+ #ifdef xen_HOST_OS
+     setXenPutStr,
+     setXenGetChar,
+ #endif
+ 
  #if !defined(__NHC__) && !defined(__HUGS__)
      -- * Unicode encoding\/decoding
  
***************
*** 228,236 ****
--- 234,244 ----
  import Data.Maybe
  import Foreign.C.Error
  import Foreign.C.Types
+ #ifndef xen_HOST_OS
  import System.Posix.Internals
  import System.Posix.Types
  #endif
+ #endif
  
  #ifdef __GLASGOW_HASKELL__
  import GHC.Base
***************
*** 299,322 ****
--- 307,372 ----
  -- -----------------------------------------------------------------------------
  -- Standard IO
  
+ #ifdef xen_HOST_OS
+ import System.IO.Unsafe (unsafePerformIO)
+ 
+ data XenIO = XenIO
+   { xenPutStr  :: String -> IO ()
+   , xenGetChar :: IO Char
+   }
+ 
+ {-# NOINLINE xenOps #-}
+ xenOps :: IORef XenIO
+ xenOps  = unsafePerformIO $ newIORef XenIO
+   { xenPutStr  = \ _ -> return ()
+   , xenGetChar = error "System.IO.xenGetChar not set in xenOps!"
+   }
+ 
+ withXenOp :: (XenIO -> f) -> IO f
+ withXenOp p = do
+   ops <- readIORef xenOps
+   return (p ops)
+ 
+ setXenPutStr :: (String -> IO ()) -> IO ()
+ setXenPutStr f = atomicModifyIORef xenOps (\ops -> (ops { xenPutStr = f }, ()))
+ 
+ setXenGetChar :: IO Char -> IO ()
+ setXenGetChar f = atomicModifyIORef xenOps (\ops -> (ops { xenGetChar = f}, ()))
+ 
+ #endif
+ 
  #ifdef __GLASGOW_HASKELL__
  -- | Write a character to the standard output device
  -- (same as 'hPutChar' 'stdout').
  
  putChar         :: Char -> IO ()
+ #ifdef xen_HOST_OS
+ putChar c       =  withXenOp xenPutStr >>= \f -> f [c]
+ #else
  putChar c       =  hPutChar stdout c
+ #endif
  
  -- | Write a string to the standard output device
  -- (same as 'hPutStr' 'stdout').
  
  putStr          :: String -> IO ()
+ #ifdef xen_HOST_OS
+ putStr s        =  withXenOp xenPutStr >>= \f -> f s
+ #else
  putStr s        =  hPutStr stdout s
+ #endif
  
  -- | The same as 'putStr', but adds a newline character.
  
  putStrLn        :: String -> IO ()
+ #ifdef xen_HOST_os
+ putStrLn s      =  do f <- withXenOp xenPutStr
+                       f s
+                       f "\n"
+ #else
  putStrLn s      =  do putStr s
                        putChar '\n'
+ #endif
  
  -- | The 'print' function outputs a value of any printable type to the
  -- standard output device.
***************
*** 336,355 ****
--- 386,427 ----
  -- (same as 'hGetChar' 'stdin').
  
  getChar         :: IO Char
+ #ifdef xen_HOST_OS
+ getChar         =  withXenOp xenGetChar >>= id
+ #else
  getChar         =  hGetChar stdin
+ #endif
  
  -- | Read a line from the standard input device
  -- (same as 'hGetLine' 'stdin').
  
  getLine         :: IO String
+ #ifdef xen_HOST_OS
+ getLine         =  do get <- withXenOp xenGetChar
+                       let loop = get >>= \c -> case c of
+                             '\n' -> return []
+                             _    -> do
+                               rest <- loop
+                               return (c:rest)
+                       loop
+ #else
  getLine         =  hGetLine stdin
+ #endif
  
  -- | The 'getContents' operation returns all user input as a single string,
  -- which is read lazily as it is needed
  -- (same as 'hGetContents' 'stdin').
  
  getContents     :: IO String
+ #ifdef xen_HOST_OS
+ getContents     =  do get <- withXenOp xenGetChar
+                       let loop = do c    <- get
+                                     rest <- loop
+                                     return (c:rest)
+                       loop
+ #else
  getContents     =  hGetContents stdin
+ #endif
  
  -- | The 'interact' function takes a function of type @String->String@
  -- as its argument.  The entire input from the standard input device is
***************
*** 435,440 ****
--- 507,513 ----
  hPrint hdl      =  hPutStrLn hdl . show
  #endif /* !__NHC__ */
  
+ 
  -- | @'withFile' name mode act@ opens a file using 'openFile' and passes
  -- the resulting handle to the computation @act@.  The handle will be
  -- closed on exit from 'withFile', whether by normal termination or by
***************
*** 512,517 ****
--- 585,607 ----
  openBinaryTempFileWithDefaultPermissions tmp_dir template
      = openTempFile' "openBinaryTempFile" tmp_dir template True 0o666
  
+ #ifdef xen_HOST_OS
+ c_getpid = return 0
+ 
+ newtype CMode = CMode Int deriving (Num,Eq,Show,Ord)
+ 
+ o_BINARY   = error "o_BINARY not valid in HaLVM"
+ o_EXCL     = error "o_EXCL not valid in HaLVM"
+ o_NONBLOCK = error "o_NONBLOCK not valid in HaLVM"
+ o_NOCTTY   = error "o_NOCTTY not valid in HaLVM"
+ o_CREAT    = error "o_CREAT not valid in HaLVM"
+ o_RDWR     = error "o_RDWR not valid in HaLVM"
+ 
+ withFilePath = error "withFilePath not valid on HaLVM"
+ c_open       = error "c_open not valid on HaLVM"
+ 
+ #endif
+ 
  openTempFile' :: String -> FilePath -> String -> Bool -> CMode
                -> IO (FilePath, Handle)
  openTempFile' loc tmp_dir template binary mode = do
***************
*** 608,613 ****
--- 698,705 ----
  foreign import ccall "getpid" c_getpid :: IO Int
  #endif
  
+ -- xen_HOST_OS
+ 
  -- $locking
  -- Implementations should enforce as far as possible, at least locally to the
  -- Haskell process, multiple-reader single-writer locking on files.
