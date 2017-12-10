{-# LANGUAGE MultiWayIf #-}
module HaLVM.POSIX.Write()
 where


import qualified Data.ByteString as S
import           Data.Word(Word8)
import           Foreign.C.Error(eINVAL)
import           Foreign.C.Types(CSize(..),CInt(..))
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

halvm_syscall_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
halvm_syscall_write fd buf amt =
  withFileDescriptorEntry_ fd $ \ ent ->
    case descType ent of
      DescConsole  con -> safeWrite Con.write con
      DescFile     fle -> safeWrite FS.write  fle
      DescSocket   sck -> safeWrite Net.send  sck
      DescListener _   -> errnoReturn eINVAL
 where
  safeWrite action value =
    runWithEINTR $
      do bstr <- S.packCStringLen (castPtr buf, fromIntegral amt)
         res  <- action value bstr
         return (fromIntegral res)

halvm_syscall_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize
halvm_syscall_writev fd bufs numbufs =
  do vecs <- peekArray (fromIntegral numbufs) bufs
     go 0 vecs
 where
  go total [] = return (fromIntegral total)
  go total (x : rest) =
    do amt <- halvm_syscall_write fd (iovBase x) (iovLen x)
       if | amt < 0 ->
              return amt
          | amt < (fromIntegral (iovLen x)) ->
              return (total + amt)
          | otherwise ->
              go (total + amt) rest

foreign export ccall halvm_syscall_write ::
  CInt -> Ptr Word8 -> CSize -> IO CSsize

foreign export ccall halvm_syscall_writev ::
  CInt -> Ptr IOVec -> CInt -> IO CSsize

