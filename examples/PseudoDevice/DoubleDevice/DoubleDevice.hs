-- The implementation of the "Double Device". This example shows how
-- developers can use the RingBuffer interface to create new device
-- drivers on top of the HALVM.
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--

import DoubleCommon
import Hypervisor.Kernel
import XenDevice.Console
import XenDevice.Xenbus
import RendezvousLib.ClientServer(accept)
import Control.Monad

main = halvm_kernel_daemon [dConsole, dXenbus] main'

main' :: [String] -> IO ()
main' _ = do
  l <- listener (const doubler)
  forever $ accept l


doubler :: DoubleRequest -> IO DoubleResponse
doubler (DoubleRequest id val) =
  return $ DoubleResponse id (val * 2)

