-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- Experimental child code demonstrating static IVC channels.

import Hypervisor.Kernel(halvm_kernel)
import Hypervisor.Debug(writeDebugConsole)
import XenDevice.Console(dConsole,getLnConsole,writeConsole)
import Data.Char(toUpper)

main :: IO ()
main = halvm_kernel [dConsole] $ const $
  do writeDebugConsole $ "consoleChild started\n"
     sequence_ $ repeat $
       do l <- getLnConsole
          writeDebugConsole $ "got: "++l++"\n"
          writeConsole $ map toUpper l ++ "\n"
