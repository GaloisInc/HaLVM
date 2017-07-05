module HaLVM.POSIX.FileDescriptors( DescriptorEntry(..)
       , newSocketFD
       , newListenerFD
       , withFileDescriptorEntry
       )
 where

import           Control.Concurrent.MVar(MVar, newMVar, modifyMVar, withMVar)
import           Data.Array.IO(IOArray)
import           Data.Array.MArray(newArray,getBounds,readArray,writeArray)
import           Foreign.C.Error(eBADF,eINVAL)
import           Foreign.C.Types(CInt(..))
import           HaLVM.Console      as Con
import           HaLVM.NetworkStack as Net
import           HaLVM.POSIX.Errno(errnoReturn)
import           System.IO.Unsafe(unsafePerformIO)

data DescriptorEntry = DescConsole  Console
                     | DescSocket   Socket
                     | DescListener ListenerSocket


{-# NOINLINE mDescriptorTable #-}
mDescriptorTable :: MVar (IOArray Word (Maybe DescriptorEntry))
mDescriptorTable = unsafePerformIO $
  do arr <- newArray (0,30) Nothing
     (stdin, stdout, stderr) <- Con.init
     writeArray arr 0 (Just (DescConsole stdin))
     writeArray arr 1 (Just (DescConsole stdout))
     writeArray arr 2 (Just (DescConsole stderr))
     newMVar arr

newSocketFD :: (Word -> IO Socket) -> IO Word
newSocketFD action = newDescriptor (\ x -> DescSocket `fmap` action x)

newListenerFD :: (Word -> IO ListenerSocket) -> IO Word
newListenerFD action = newDescriptor (\ x -> DescListener `fmap` action x)

newDescriptor :: (Word -> IO DescriptorEntry) -> IO Word
newDescriptor action =
  modifyMVar mDescriptorTable $ \ dt ->
    do (low, high) <- getBounds dt
       melem <- runFind low high dt
       case melem of
         Nothing ->
           do dt' <- newArray (low, high + 20) Nothing
              copyArray low high dt dt'
              newVal <- action (high + 1)
              writeArray dt' (high + 1) (Just newVal)
              return (dt', high + 1)
         Just i ->
           do newVal <- action i
              writeArray dt i (Just newVal)
              return (dt, i)

runFind :: Word -> Word -> IOArray Word (Maybe a) -> IO (Maybe Word)
runFind x y arr | x > y     = return Nothing
                | otherwise = do cur <- readArray arr x
                                 case cur of
                                   Nothing -> return (Just x)
                                   Just _  -> runFind (x + 1) y arr

copyArray :: Word -> Word -> IOArray Word a -> IOArray Word a -> IO ()
copyArray x y arr1 arr2
  | x > y     = return ()
  | otherwise =
     do v <- readArray arr1 x
        writeArray arr2 x v
        copyArray (x + 1) y arr1 arr2

withFileDescriptorEntry :: Num a => Word -> (DescriptorEntry -> IO a) -> IO a
withFileDescriptorEntry fd handler =
  withMVar mDescriptorTable $ \ dt ->
    do ent <- readArray dt fd
       case ent of
         Nothing ->
           errnoReturn eBADF
         Just desc ->
           handler desc

-- -----------------------------------------------------------------------------

syscall_dup :: CInt -> IO CInt
syscall_dup fd
  | fd < 0 = errnoReturn eINVAL
  | otherwise =
      do mydesc <- withMVar mDescriptorTable $ \ dt ->
                     readArray dt (fromIntegral fd)
         case mydesc of
           Nothing ->
             errnoReturn eBADF
           Just desc ->
             do fd' <- newDescriptor (const (return desc))
                return (fromIntegral fd')

foreign export ccall syscall_dup ::
  CInt -> IO CInt

