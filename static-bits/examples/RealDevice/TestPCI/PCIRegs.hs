-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND

module PCIRegs where

import Data.Word (Word8,Word16)

-- FIXME: Add Device ID, Vendor ID, Revision ID, Class code, subsystem ID,
-- subsystem vendor ID?

-- | PCI Device Registers
data PCIReg
  = PCI_COMMAND_REGISTER
  | PCI_STATUS_REGISTER
  | PCI_CACHE_LINE_SIZE
  | PCI_LATENCY_TIMER
  | PCI_CAPABILITY_LIST
  | PCI_INTERRUPT_LINE
  | PCI_INTERRUPT_PIN
  | PCI_MIN_GNT          -- ^ Minimum "grant": only applies to bus master
                         -- devices; time the master would like retain
                         -- ownership of the PCI bus
  | PCI_MAX_LATENCY
  deriving (Show,Eq)

-- | Get the offset of a device register within the PCI device/functon
-- configuration header.
fromReg :: PCIReg -> Word8
fromReg reg = case reg of
  PCI_COMMAND_REGISTER -> 0x04
  PCI_STATUS_REGISTER  -> 0x06
  PCI_CACHE_LINE_SIZE  -> 0x0c
  PCI_LATENCY_TIMER    -> 0x0d
  PCI_CAPABILITY_LIST  -> 0x34
  PCI_INTERRUPT_LINE   -> 0x3c
  PCI_INTERRUPT_PIN    -> 0x3d
  PCI_MIN_GNT          -> 0x3e
  PCI_MAX_LATENCY      -> 0x3f

-- | Command Register Values
data PCICmd
  = PCI_COMMAND_MASTER

fromCmd :: PCICmd -> Word16
fromCmd cmd = case cmd of
  PCI_COMMAND_MASTER -> 0x4

