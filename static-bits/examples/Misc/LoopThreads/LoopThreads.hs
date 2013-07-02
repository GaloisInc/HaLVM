-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND

import Hypervisor.Debug
import Control.Concurrent

main = do
  forkIO (say 30 "AAAAAA\n")
  say 30 "BBBBBBB\n"

say :: Int -> String -> IO ()
say 0 s = return ()
say x s = do writeDebugConsole s
             threadDelay 10000
             say (x - 1) s
