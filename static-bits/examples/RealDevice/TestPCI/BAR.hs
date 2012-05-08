-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Trevor Elliott <trevor@galois.com>
-- BANNEREND

module BAR (
    barLenPages
  , getBARSize
  , mapBAR
  ) where

import Data.Bits (Bits((.&.),(.|.),complement,shiftR))
import Data.Word (Word8,Word32)
import Device.PCI.BaseAddr (getBAR,AddrType(Mem),addr)
import Device.PCI.ConfigSpace (get32,set32)
import Device.PCI.Device (Dev,config)
import Hypervisor.Basics (DomId(..),Xen,Err(ENXIO),xThrow)
import Hypervisor.Memory (toMFN,mapForeignMachineFrames,VPtr)

pciRomAddrMask :: Word32
pciRomAddrMask = complement 0x7ff

-- | Turn the bar length into a number of pages.
barLenPages :: Integral a => a -> a
barLenPages bytes = case quotRem bytes 0x1000 of
  (n,0) -> n
  (n,_) -> n + 1

-- | Gets the size of the memory location referenced by the BAR, in bytes.
getBARSize :: Dev a -> Word8 -> IO Word32
getBARSize dev bar = do
  let c   = config dev
  let off = 0x10 + bar
  l <- get32 c off
  set32 c off 0xffffffff
  maxBase <- get32 c off
  set32 c off l
  let base = pciRomAddrMask .&. l
  case pciRomAddrMask .&. maxBase of
    0    -> return 0
    size -> do
      let size' = (size .&. complement (size - 1)) - 1
      if base == maxBase
         && ((base .|. size') .&. pciRomAddrMask) /= pciRomAddrMask
         then return 0
         else return size'

-- | Map the memory referenced by a BAR into virtual memory.
mapBAR :: Dev a -> Word8 -> Xen (VPtr Word32)
mapBAR dev bar = do
  Mem mem <- getBAR dev bar
  size    <- getBARSize dev bar
  case barLenPages size of
    0     -> xThrow ENXIO
    pages -> mapForeignMachineFrames (DomId 0) mfns
      where mfns = [ toMFN (fromIntegral $ (addr mem `shiftR` 12) + i)
                   | i <- [0 .. pages - 1] ]
