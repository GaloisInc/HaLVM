-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--

import Hypervisor.Kernel
import Hypervisor.Privileged
import XenDevice.Console

main :: IO ()
main = halvm_kernel_daemon [dConsole] $ const $ do
  writeConsole "Getting ID ...\n"
  me <- myDomId 
  writeConsole $ "Got ID = " ++ show me ++ ".\n"

