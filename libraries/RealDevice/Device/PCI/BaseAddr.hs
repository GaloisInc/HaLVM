-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.PCI.BaseAddr 
  ( AddrType(..), IOAddr(..), MemAddr, getBAR
  , addr, memType, prefetch
  , ioOffset
  ) where

import Device.PCI.Device
import Device.PCI.ConfigSpace

import Numeric (showHex)
import Data.Word
import Data.Bits

-- bitdata MemType  = Mem32 as B00
--                  | Mem1M as B01
--                  | Mem64 as B10
-- 
-- bitdata BaseAddr = Mem { addr      = addr' # B0000 :: Bit 32
--                        , prefetch  :: Bit 1
--                        , memType   :: MemType
--                        }
--                    as addr' # prefetch # memType # B0
-- 
--                  | IO { addr = addr' # B00 :: Bit 32 }
--                    as addr' # (_ :: Bit 1) # B1 


-- XXX: Should make it abstract at some point.
newtype IOAddr      = I Word32
newtype MemAddr     = M Word32

-- in bytes
ioOffset :: IOAddr -> Word32 -> IOAddr
ioOffset (I x) o = I (x + o)

data AddrType       = IO IOAddr
                    | Mem MemAddr
                      deriving Show

getBAR             :: Dev a -> Word8 -> IO AddrType
getBAR d n          = do b <- config d `get32` (0x10 + (4*n))-- XXX: make safer
                         return $ if b `testBit` 0 
                                     then IO (I b)
                                     else Mem (M b)

class HasAddr t where
  addr             :: t -> Word32

-- hmm.. 8029 suggests that for IO, bits 2,3 are
-- also not part of the address...
instance HasAddr IOAddr where
  addr (I x)        = x .&. complement 0xF {-0x3-}

instance HasAddr MemAddr where
  addr (M x)        = x .&. complement 0xF  


prefetch           :: MemAddr -> Bool
prefetch (M x)      = x `testBit` 3

memType            :: MemAddr -> Word32
memType (M x)       = (x `shiftR` 1) .&. 0x3

instance Show MemAddr where
  show x            = "0x" ++ showHex (addr x) ""
                   ++ if prefetch x then "(pre)" else ""

instance Show IOAddr where
  show x            = "0x" ++ showHex (addr x) ""





-- 00111111     = 127
-- 00111100     = 124
-- 00000100     = 4
-- 00000011     = 3


-- 00100001     = 65
-- 00100000     = 64
-- 00100000     = 64
-- 00011111     = 63


-- 2s complement:  -x = ~(x-1)
-- what is: x & -x  ?
-- only keeps the least significant 1 of x:

-- 00000001 & 11111111 = 00000001
-- 00000010 & 11111110 = 00000010
-- 00000011 & 11111101 = 00000001
-- 00000100 & 11111100 = 00000100
-- 00000101 & 11111011 = 00000001
-- 00000110 & 11111010 = 00000010



