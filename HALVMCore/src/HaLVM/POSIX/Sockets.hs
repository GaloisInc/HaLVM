{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HaLVM.POSIX.Sockets()
 where

import           Control.Monad(when)
import           Data.Bits((.&.))
import           Data.Word(Word16,Word32)
import           Foreign.C.Types(CInt(..))
import           Foreign.Ptr(Ptr,castPtr)
import           Foreign.Storable(Storable(..))
import           HaLVM.NetworkStack as Net

data SockAddr = SockAddr {
       saFamily :: Word16
     , saPort   :: Net.Port
     , saAddr   :: Net.Addr
     }

instance Storable SockAddr where
  sizeOf    _   = 16
  alignment _   = 1
  peek      p   = do fam  <- peekByteOff (castPtr p) 0
                     port <- peekByteOff (castPtr p) 2
                     addr <- peekByteOff (castPtr p) 4
                     return (SockAddr fam port addr)
  poke      p v = do pokeByteOff (castPtr p) 0 (saFamily v)
                     pokeByteOff (castPtr p) 2 (saPort v)
                     pokeByteOff (castPtr p) 4 (saAddr v)

newtype SockLen = SockLen Word32
  deriving (Eq, Num, Storable)

syscall_accept :: CInt -> Ptr SockAddr -> Ptr SockLen -> IO CInt
syscall_accept fd paddr plen = undefined fd paddr plen

syscall_accept4 :: CInt -> Ptr SockAddr -> Ptr SockLen -> CInt -> IO CInt
syscall_accept4 fd paddr plen flags =
  do res <- syscall_accept fd paddr plen
     when (res > 0) $
       do when (flags .&. sockCLOEXEC /= 0) $
            undefined
          when (flags .&. sockNONBLOCK /= 0) $
            undefined
     return res

sockCLOEXEC :: CInt
sockCLOEXEC = 0o2000000

sockNONBLOCK :: CInt
sockNONBLOCK = 0o4000

foreign export ccall syscall_accept ::
  CInt -> Ptr SockAddr -> Ptr SockLen -> IO CInt

foreign export ccall syscall_accept4 ::
  CInt -> Ptr SockAddr -> Ptr SockLen -> CInt -> IO CInt

