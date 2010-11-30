*** ghc-pristine/libraries/template-haskell/Language/Haskell/TH/Syntax.hs	2010-06-09 11:10:21.000000000 -0700
--- xen-ghc/libraries/template-haskell/Language/Haskell/TH/Syntax.hs	2010-07-02 13:41:27.109652708 -0700
***************
*** 57,63 ****
--- 57,65 ----
  import Data.IORef
  import System.IO.Unsafe	( unsafePerformIO )
  import Control.Monad (liftM)
+ #ifndef xen_HOST_OS
  import System.IO	( hPutStrLn, stderr )
+ #endif
  import Data.Char        ( isAlpha )
  
  -----------------------------------------------------
***************
*** 100,108 ****
                   ; writeIORef counter (n+1)
                   ; return (mkNameU s n) }
  
    qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
    qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
! 
    qReify _     = badIO "reify"
    qLocation    = badIO "currentLocation"
    qRecover _ _ = badIO "recover" -- Maybe we could fix this?
--- 102,114 ----
                   ; writeIORef counter (n+1)
                   ; return (mkNameU s n) }
  
+ #ifdef xen_HOST_OS
+   qReport True  msg = fail ("Template Haskell error: " ++ msg)
+   qReport False msg = fail ("Template Haskell error: " ++ msg)
+ #else 
    qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
    qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
! #endif
    qReify _     = badIO "reify"
    qLocation    = badIO "currentLocation"
    qRecover _ _ = badIO "recover" -- Maybe we could fix this?
