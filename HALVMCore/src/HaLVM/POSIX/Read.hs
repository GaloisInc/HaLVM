{-# LANGUAGE MultiWayIf #-}
module HaLVM.POSIX.Read()
 where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S
import           Data.Word(Word8)
import           Foreign.C.Error(eINVAL)
import           Foreign.C.Types(CSize(..),CInt(..))
import           Foreign.Marshal.Utils(copyBytes)
import           Foreign.Marshal.Array(peekArray)
import           Foreign.Ptr(Ptr,castPtr)
import           HaLVM.Console      as Con
import           HaLVM.NetworkStack as Net
import           HaLVM.POSIX.Errno(errnoReturn,runWithEINTR)
import           HaLVM.POSIX.IOVec(IOVec(..))
import           HaLVM.POSIX.FileDescriptors(DescriptorEntry(..),
                                             DescriptorType(..),
                                             withFileDescriptorEntry_)
import           System.Posix.Types(CSsize(..))

syscall_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
syscall_read fd buf amt
  | fd < 0 = errnoReturn eINVAL
  | otherwise =
      withFileDescriptorEntry_ (fromIntegral fd) $ \ ent ->
        case descType ent of
          DescConsole con ->
            runWithEINTR $
              do res <- Con.read con (fromIntegral amt)
                 S.unsafeUseAsCStringLen res $ \ (ptr, len) ->
                   do copyBytes buf (castPtr ptr) len
                      return (fromIntegral len)
          DescSocket sock ->
            runWithEINTR $
              do res <- Net.recv sock (fromIntegral amt)
                 S.unsafeUseAsCStringLen (L.toStrict res) $ \ (ptr, len) ->
                   do copyBytes buf (castPtr ptr) len
                      return (fromIntegral len)
          DescListener _ ->
             errnoReturn eINVAL

syscall_readv :: CInt -> Ptr IOVec -> CInt -> IO CSsize
syscall_readv fd bufs numbufs =
  do vecs <- peekArray (fromIntegral numbufs) bufs
     go 0 vecs
 where
  go total [] = return (fromIntegral total)
  go total (x : rest) =
    do amt <- syscall_read fd (iovBase x) (iovLen x)
       if | amt < 0 ->
              return amt
          | amt < (fromIntegral (iovLen x)) ->
              return (total + amt)
          | otherwise ->
              go (total + amt) rest

foreign export ccall syscall_read ::
  CInt -> Ptr Word8 -> CSize -> IO CSsize

foreign export ccall syscall_readv ::
  CInt -> Ptr IOVec -> CInt -> IO CSsize

