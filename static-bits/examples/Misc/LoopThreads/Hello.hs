-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND

import Hypervisor.Kernel
import Hypervisor.Debug
import Control.Concurrent

main = halvm_kernel [] $ const $ forkIO (say "AAAAAA\n") >> say "BBBBBBB\n"

say x = do writeDebugConsole x
           threadDelay 10000
           say x
