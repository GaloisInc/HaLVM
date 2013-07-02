-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--
import Control.Concurrent
import Hypervisor.Console
import Hypervisor.XenStore

main :: IO ()
main = do
  con <- initXenConsole
  xs  <- initXenStore
  writeConsole con "Getting ID ...\n"
  me <- xsGetDomId xs
  writeConsole con $ "Got ID = " ++ show me ++ ".\n"
  threadDelay (2 * 1000 * 1000)
