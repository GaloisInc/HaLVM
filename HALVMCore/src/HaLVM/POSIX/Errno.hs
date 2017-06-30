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
errnoReturn err =
  do setErrno err
     return (-1)

runWithEINTR :: Num b => IO b -> IO b
runWithEINTR = handle nevermind
 where
  nevermind :: Num a => SomeException -> IO a
  nevermind _  = do setErrno eINTR
                    return (-1)


setErrno :: Errno -> IO ()
setErrno (Errno v) = set_errno v

foreign import ccall unsafe
  set_errno :: CInt -> IO ()
