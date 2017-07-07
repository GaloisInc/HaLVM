module HaLVM.POSIX.FileDescriptors(
         DescriptorEntry(..)
       , DescriptorType(..)
       , dup
       , withFileDescriptorEntry
       , withFileDescriptorEntry_
       , removeDescriptor
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

data DescriptorEntry = DescriptorEntry {
       descCloseOnExec :: Bool
     , descStatusFlags :: CInt
     , descType        :: DescriptorType
     }

buildDescriptor :: DescriptorType -> DescriptorEntry
buildDescriptor t = DescriptorEntry False 0 t

data DescriptorType = DescConsole  Console
                    | DescSocket   Socket
                    | DescListener ListenerSocket


{-# NOINLINE mDescriptorTable #-}
mDescriptorTable :: MVar (IOArray Word (Maybe DescriptorEntry))
mDescriptorTable = unsafePerformIO $
  do arr <- newArray (0,30) Nothing
     (stdin, stdout, stderr) <- Con.init
     writeArray arr 0 (Just (buildDescriptor (DescConsole stdin)))
     writeArray arr 1 (Just (buildDescriptor (DescConsole stdout)))
     writeArray arr 2 (Just (buildDescriptor (DescConsole stderr)))
     newMVar arr

newDescriptor :: (Word -> Bool) -> (Word -> IO DescriptorEntry) -> IO Word
newDescriptor validFd action =
  modifyMVar mDescriptorTable $ \ dt ->
    do (low, high) <- getBounds dt
       melem <- runFind validFd low high dt
       case melem of
         Nothing ->
           do let firstGoodValue = head (filter validFd [high+1..])
              dt' <- newArray (low, firstGoodValue + 20) Nothing
              copyArray low high dt dt'
              newVal <- action firstGoodValue
              writeArray dt' firstGoodValue (Just newVal)
              return (dt', firstGoodValue)
         Just i ->
           do newVal <- action i
              writeArray dt i (Just newVal)
              return (dt, i)

-- |Remove a descriptor. WARNING: This does not properly close the descriptor,
-- or deallocate any data structures. It just removes it from our file
-- descriptor table. You probably shouldn't use this.
removeDescriptor :: Word -> IO ()
removeDescriptor fd =
  withMVar mDescriptorTable $ \ dt ->
    writeArray dt fd Nothing

runFind :: (Word -> Bool) -> Word -> Word -> IOArray Word (Maybe a) -> IO (Maybe Word)
runFind validFd x y arr
  | x > y            = return Nothing
  | not (validFd x)  = runFind validFd (x + 1) y arr
  | otherwise        = do cur <- readArray arr x
                          case cur of
                            Nothing -> return (Just x)
                            Just _  -> runFind validFd (x + 1) y arr

copyArray :: Word -> Word -> IOArray Word a -> IOArray Word a -> IO ()
copyArray x y arr1 arr2
  | x > y     = return ()
  | otherwise =
     do v <- readArray arr1 x
        writeArray arr2 x v
        copyArray (x + 1) y arr1 arr2

withFileDescriptorEntry :: Num a => Word -> (DescriptorEntry -> IO (DescriptorEntry, a)) -> IO a
withFileDescriptorEntry fd handler =
  withMVar mDescriptorTable $ \ dt ->
    do ent <- readArray dt fd
       case ent of
         Nothing ->
           errnoReturn eBADF
         Just desc ->
           do (desc', res) <- handler desc
              writeArray dt fd (Just desc')
              return res

withFileDescriptorEntry_ :: Num a => Word -> (DescriptorEntry -> IO a) -> IO a
withFileDescriptorEntry_ fd handler =
  withFileDescriptorEntry fd $ \ ent ->
    do res <- handler ent
       return (ent, res)

-- -----------------------------------------------------------------------------

-- |Duplicate the file descriptor in the second argument, ensuring that the
-- new file descriptor passes the check in the first argument.
dup :: (Word -> Bool) -> Word -> IO Word
dup fdOk fd =
  do mdsc <- withMVar mDescriptorTable $ \ dt -> readArray dt (fromIntegral fd)
     case mdsc of
       Nothing ->
         errnoReturn eBADF
       Just desc ->
         do fd' <- newDescriptor fdOk (const (return desc))
            return (fromIntegral fd')


syscall_dup :: CInt -> IO CInt
syscall_dup fd
  | fd < 0    = errnoReturn eINVAL
  | otherwise = fromIntegral `fmap` dup (const True) (fromIntegral fd)

foreign export ccall syscall_dup ::
  CInt -> IO CInt

