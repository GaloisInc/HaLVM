-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND

import Control.Exception
import Hypervisor.Debug
import Hypervisor.Basics
import Communication.IVC
import Hypervisor.Kernel
import Hypervisor.Memory
import XenDevice.Xenbus
import XenDevice.Console
import Common

main :: IO ()
main = halvm_kernel_daemon [dXenbus,dConsole] start

start :: [String] -> IO ()
start args = do
  c <- accept
  writer "SND: Waiting for reference.\n"
  ref <- get c
  writer "SND: Making page.\n"
  let dom = peer c
  page <- makePageData dom
  writer "SND: Sending page.\n"
  res <- xTry $ transferPageToForeignDomain page dom ref
  writer "SND: Page sent.\n"
  assert (isRight res) $ return ()
  writer "SND: Done.\n"
 where
  isRight (Left _) = False
  isRight (Right _) = True

  writer = case args of
             ["writer=console"] -> \ s -> do
                                      writeConsole s
                                      writeDebugConsole s
             _                  -> writeDebugConsole

