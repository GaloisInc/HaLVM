module HaLVM.POSIX.Errno(
         errnoReturn
       , runWithEINTR
       , setErrno
       )
 where

import Control.Exception(SomeException, handle)
import Foreign.C.Error(Errno(..),eINTR)
import Foreign.C.Types(CInt(..))

errnoReturn :: Num a => Errno -> IO a
errnoReturn (Errno v) = return (fromIntegral (- v))

runWithEINTR :: Num a => IO a -> IO a
runWithEINTR = handle nevermind
 where
  nevermind :: Num a => SomeException -> IO a
  nevermind _  = errnoReturn eINTR

setErrno :: Errno -> IO ()
setErrno (Errno v) = set_errno v

foreign import ccall unsafe
  set_errno :: CInt -> IO ()
