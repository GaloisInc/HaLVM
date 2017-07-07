{-# LANGUAGE MultiWayIf #-}
module HaLVM.POSIX.Write()
 where


import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Word(Word8)
import           Foreign.C.Error(eINVAL)
import           Foreign.C.Types(CSize(..),CInt(..))
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

syscall_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
syscall_write fd buf amt
  | fd < 0 = errnoReturn eINVAL
  | otherwise =
      withFileDescriptorEntry_ (fromIntegral fd) $ \ ent ->
        case descType ent of
          DescConsole con ->
            runWithEINTR $
              do bstr <- S.packCStringLen (castPtr buf, fromIntegral amt)
                 res  <- Con.write con bstr
                 return (fromIntegral res)
          DescSocket sock ->
            runWithEINTR $
              do bstr <- S.packCStringLen (castPtr buf, fromIntegral amt)
                 let lbstr = L.fromStrict bstr
                 res  <- Net.send sock lbstr
                 return (fromIntegral res)
          DescListener _ ->
             errnoReturn eINVAL

syscall_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize
syscall_writev fd bufs numbufs =
  do vecs <- peekArray (fromIntegral numbufs) bufs
     go 0 vecs
 where
  go total [] = return (fromIntegral total)
  go total (x : rest) =
    do amt <- syscall_write fd (iovBase x) (iovLen x)
       if | amt < 0 ->
              return amt
          | amt < (fromIntegral (iovLen x)) ->
              return (total + amt)
          | otherwise ->
              go (total + amt) rest

foreign export ccall syscall_write ::
  CInt -> Ptr Word8 -> CSize -> IO CSsize

foreign export ccall syscall_writev ::
  CInt -> Ptr IOVec -> CInt -> IO CSsize

