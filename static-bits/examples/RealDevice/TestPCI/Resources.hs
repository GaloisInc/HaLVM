-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND

module Resources where

import PCIRegs

import Control.Monad (unless)
import Data.Bits (Bits((.|.)))
import Data.Word (Word16)
import Device.PCI.ConfigSpace (get16,set16)
import Device.PCI.Device (Dev,config)

enableResources :: Dev a -> IO ()
enableResources dev = do
  cmd <- get16 (config dev) (fromReg PCI_COMMAND_REGISTER)
  let new = cmd .|. 0x3
  unless (cmd == new) (set16 (config dev) (fromReg PCI_COMMAND_REGISTER) new)

