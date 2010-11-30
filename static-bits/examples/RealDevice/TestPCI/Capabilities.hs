-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND

module Capabilities where

import PCIRegs

import Data.Word (Word8,Word16)
import Device.PCI.ConfigSpace (get8,set8,get16,set16)
import Device.PCI.Device (Dev,devData,config)

-- PCI Capabilities ------------------------------------------------------------

data Capability
  = PowerManagement Word8
  deriving Show

isPM :: Capability -> Bool
isPM (PowerManagement _) = True
isPM _                   = False

-- | Get the capabilities for a pci device.
getCapabilities :: Dev a -> IO [Capability]
getCapabilities dev = loop =<< get8 (config dev) (fromReg PCI_CAPABILITY_LIST)
  where
  loop 0x0 = return []
  loop off = do
    ty   <- get8 (config dev)  off
    next <- get8 (config dev) (off + 1)
    rest <- loop next
    return $! case ty of
      0x01 -> PowerManagement (off + 0x4) : rest
      _    ->                               rest

-- | Devices that provide a PM register.
class PM a where
  pmReg :: a -> Word8

-- | Get the PM register.
getPmReg :: PM a => Dev a -> IO Word16
getPmReg d = get16 (config d) (pmReg (devData d))

-- | Set the PM register.
setPmReg :: PM a => Dev a -> Word16 -> IO ()
setPmReg d = set16 (config d) (pmReg (devData d))
