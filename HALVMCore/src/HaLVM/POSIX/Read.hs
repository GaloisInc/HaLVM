{-# LANGUAGE MultiWayIf #-}
module HaLVM.POSIX.Read()
 where

import qualified Data.ByteString.Unsafe as S
import           Data.Word(Word8)
import           Foreign.C.Error(eINVAL)
import           Foreign.C.Types(CSize(..),CInt(..))
import           Foreign.Marshal.Utils(copyBytes)
import           Foreign.Marshal.Array(peekArray)
import           Foreign.Ptr(Ptr,castPtr)
import           HaLVM.Console      as Con
import           HaLVM.FileSystem   as FS
import           HaLVM.NetworkStack as Net
import           HaLVM.POSIX.Errno(errnoReturn,runWithEINTR)
import           HaLVM.POSIX.IOVec(IOVec(..))
import           HaLVM.POSIX.FileDescriptors(DescriptorEntry(..),
                                             DescriptorType(..),
                                             withFileDescriptorEntry_)
import           System.Posix.Types(CSsize(..))

halvm_syscall_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
halvm_syscall_read fd buf amt =
  withFileDescriptorEntry_ fd $ \ ent ->
    case descType ent of
      DescConsole  con -> safeRead Con.read con
      DescSocket   skt -> safeRead Net.recv skt
      DescListener _   -> errnoReturn eINVAL
      DescFile     fle -> safeRead FS.read fle
 where
  safeRead action value =
    runWithEINTR $
      do res <- action value (fromIntegral amt)
         S.unsafeUseAsCStringLen res $ \ (ptr, len) ->
            do copyBytes buf (castPtr ptr) len
               return (fromIntegral len)

halvm_syscall_readv :: CInt -> Ptr IOVec -> CInt -> IO CSsize
halvm_syscall_readv fd bufs numbufs =
  do vecs <- peekArray (fromIntegral numbufs) bufs
     go 0 vecs
 where
  go total [] = return (fromIntegral total)
  go total (x : rest) =
    do amt <- halvm_syscall_read fd (iovBase x) (iovLen x)
       if | amt < 0 ->
              return amt
          | amt < (fromIntegral (iovLen x)) ->
              return (total + amt)
          | otherwise ->
              go (total + amt) rest

foreign export ccall halvm_syscall_read ::
  CInt -> Ptr Word8 -> CSize -> IO CSsize

foreign export ccall halvm_syscall_readv ::
  CInt -> Ptr IOVec -> CInt -> IO CSsize

