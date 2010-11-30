-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Common
import Control.Concurrent
import Control.Exception
import Hypervisor.Basics
import Hypervisor.Debug
import Communication.IVC
import Hypervisor.Kernel
import Hypervisor.Memory
import XenDevice.Xenbus
import XenDevice.Console

main :: IO ()
main = halvm_kernel_daemon [dXenbus,dConsole] start

start :: [String] -> IO ()
start args = do
  writer "SND: Waiting for accept.\n"
  c <- accept
  writer "SND: Waiting for reference.\n"
  ref <- get c
  writer "SND: Making page.\n"
  let dom = peer c
  page <- makePageData
  writer "SND: Copying page.\n"
  mfn <- vptrToMFN page
  res <- performFrameCopy (Right mfn) domidSelf 0 (Left ref) dom 0 4096
  writer "SND: Page copied.\n"
  writer "SND: Done.\n"
  threadDelay 10000
 where
  writer = case args of
             ["writer=console"] -> writeConsole
             _                  -> writeDebugConsole
