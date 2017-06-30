module HaLVM.POSIX.IOVec(
         IOVec(..)
       )
 where

import Data.Word(Word8)
import Foreign.Ptr(Ptr,castPtr,plusPtr)
import Foreign.Storable(Storable(..))
import Foreign.C.Types(CSize(..))

data IOVec = IOVec {
       iovBase :: Ptr Word8
     , iovLen  :: CSize
     }

instance Storable IOVec where
  sizeOf _    = sizeOf (undefined :: Ptr Word8) +
                sizeOf (undefined :: CSize)
  alignment _ = max (alignment (undefined :: Ptr Word8))
                    (alignment (undefined :: CSize))
  peek ptr    = do base <- peek (castPtr ptr)
                   let lenptr = ptr `plusPtr` (sizeOf (undefined :: Ptr Word8))
                   len  <- peek (castPtr lenptr)
                   return (IOVec base len)
  poke ptr v  = do poke (castPtr ptr) (iovBase v)
                   let lenptr = ptr `plusPtr` (sizeOf (undefined :: Ptr Word8))
                   poke (castPtr lenptr) (iovLen v)


