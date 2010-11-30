-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.Utils where

import Data.Word(Word32)
import Hypervisor.Basics(Xen)
import Hypervisor.Port(bindPhysicalIRQ,setPortHandler)

type IRQ = Word32

-- | Set the handler for an interrupt.
-- Inputs:
--   * The interrupt number
--   * Is the interrupt shared?
--   * An IO action to be invoked upon receiving an interrput.
-- Output:
--   * Boolean flag indicating of we succeeded.
set_interrupt :: IRQ -> Bool -> IO () -> Xen Bool
set_interrupt irq shared handler =
  do port <- bindPhysicalIRQ irq shared
     setPortHandler port handler >> return True

