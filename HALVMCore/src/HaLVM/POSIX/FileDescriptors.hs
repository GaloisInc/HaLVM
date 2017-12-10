module HaLVM.POSIX.FileDescriptors(
         DescriptorEntry(..)
       , DescriptorType(..)
       , addFd
       , withFileDescriptorEntry, withFileDescriptorEntry_
       , dup
       , removeDescriptor
       )
 where

import           Control.Concurrent.MVar(MVar, newMVar, modifyMVar, withMVar)
import           Control.Monad(when)
import           Data.Array.IO(IOArray)
import           Data.Array.MArray(newArray,getBounds,readArray,writeArray)
import           Data.Bits(testBit)
import           Foreign.C.Error(eBADF,eINVAL)
import           Foreign.C.Types(CInt(..))
import           HaLVM.Console      as Con
import           HaLVM.FileSystem   as FS
import           HaLVM.NetworkStack as Net
import           HaLVM.POSIX.Errno(errnoReturn)
import           System.IO.Unsafe(unsafePerformIO)

data DescriptorEntry = DescriptorEntry {
       descCloseOnExec :: Bool
     , descStatusFlags :: CInt
     , descType        :: DescriptorType
     }

data DescriptorType = DescConsole  Console
                    | DescFile     FS.File
                    | DescListener ListenerSocket
                    | DescSocket   Socket


buildDescriptor :: DescriptorType -> DescriptorEntry
buildDescriptor t = DescriptorEntry False 0 t

closeDescriptor :: DescriptorEntry -> IO ()
closeDescriptor de =
  case descType de of
    DescConsole  _ -> return ()
    DescSocket   s -> close s
    DescListener s -> closeListener s

-- -----------------------------------------------------------------------------

type DescriptorTable = IOArray Word (Maybe DescriptorEntry)

{-# NOINLINE mDescriptorTable #-}
mDescriptorTable :: MVar (IOArray Word (Maybe DescriptorEntry))
mDescriptorTable = unsafePerformIO $
  do arr <- newArray (0,30) Nothing
     (stdin, stdout, stderr) <- Con.init
     writeArray arr 0 (Just (buildDescriptor (DescConsole stdin)))
     writeArray arr 1 (Just (buildDescriptor (DescConsole stdout)))
     writeArray arr 2 (Just (buildDescriptor (DescConsole stderr)))
     newMVar arr

newFd :: (Word -> Bool) -> DescriptorTable -> IO (Word, DescriptorTable)
newFd check dt =
  do (low, high) <- getBounds dt
     go low high
 where
   go :: Word -> Word -> IO (Word, DescriptorTable)
   go idx high
     | not (check idx) =
         go (idx + 1) high
     -- NOTE: Don't rearrange these cases. We want to do the check first,
     -- to force idx high enough that it's definitely OK if we need to
     -- resize the array.
     | idx > high =
         do (low, _) <- getBounds dt
            dt' <- newArray (low, high + 30) Nothing
            copyArray low high dt dt'
            return (idx, dt')
     | otherwise =
         do ent <- readArray dt idx
            case ent of
              Nothing -> return (idx, dt)
              Just _  -> go (idx + 1) high
   --
   copyArray x y arr1 arr2 =
     when (x <= y) $
       do v <- readArray arr1 x
          writeArray arr2 x v
          copyArray (x + 1) y arr1 arr2

addFd :: DescriptorEntry -> IO Word
addFd ent =
  modifyMVar mDescriptorTable $ \ descTable ->
    do (res, descTable') <- newFd (const True) descTable
       writeArray descTable res (Just ent)
       return (descTable', res)

-- |Remove a descriptor. WARNING: This does not properly close the descriptor,
-- or deallocate any data structures. It just removes it from our file
-- descriptor table. You probably shouldn't use this.
removeDescriptor :: Word -> IO ()
removeDescriptor fd =
  withMVar mDescriptorTable $ \ dt ->
    writeArray dt fd Nothing

-- -----------------------------------------------------------------------------

withFileDescriptorEntry :: Num a =>
                           Word ->
                           (DescriptorEntry -> IO (DescriptorEntry, a)) ->
                           IO a
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
dup :: (Word -> Bool) -> Word -> Maybe Word -> CInt -> IO CInt
dup check oldfd mnewfd flags =
  modifyMVar mDescriptorTable $ \ descTable ->
    do origDesc <- readArray descTable oldfd
       case origDesc of
         Nothing ->
           do res <- errnoReturn eBADF
              return (descTable, res)
         Just desc ->
           do (newfd, descTable') <- getDupFileDesc mnewfd descTable
              writeArray descTable' newfd (Just desc{
                  descCloseOnExec = testBit flags 19 -- FIXME ?
                })
              return (descTable', fromIntegral newfd)
 where
  getDupFileDesc Nothing   table = newFd check table
  getDupFileDesc (Just fd) table =
    do curval <- readArray table fd
       case curval of
         Nothing -> return ()
         Just v  -> closeDescriptor v >> writeArray table fd Nothing
       return (fd, table)

dup' :: Word -> Maybe Word -> CInt -> IO CInt
dup' = dup (const True)

type DupType = CInt -> IO CInt
foreign export ccall halvm_syscall_dup :: DupType
halvm_syscall_dup :: DupType
halvm_syscall_dup fd
  | fd < 0    = errnoReturn eINVAL
  | otherwise = dup' (fromIntegral fd) Nothing 0

type Dup2Type = CInt -> CInt -> IO CInt
foreign export ccall halvm_syscall_dup2 :: Dup2Type
halvm_syscall_dup2 :: Dup2Type
halvm_syscall_dup2 oldfd newfd
  | oldfd < 0      = errnoReturn eINVAL
  | oldfd == newfd = return newfd
  | otherwise      = dup' (fromIntegral oldfd) (Just (fromIntegral newfd)) 0

type Dup3Type = CInt -> CInt -> CInt -> IO CInt
foreign export ccall halvm_syscall_dup3 :: Dup3Type
halvm_syscall_dup3 :: Dup3Type
halvm_syscall_dup3 oldfd newfd flags
  | oldfd < 0      = errnoReturn eINVAL
  | oldfd == newfd = errnoReturn eINVAL
  | otherwise      = dup' (fromIntegral oldfd) (Just (fromIntegral newfd)) flags

