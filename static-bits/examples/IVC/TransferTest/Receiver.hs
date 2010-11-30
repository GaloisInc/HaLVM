-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Control.Concurrent
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
  writer "RCV: Making offer.\n"
  c <- offer
  writer "RCV: Creating reference & granting access.\n"
  let dom = peer c
  Right ref <- xTry $ allocRef
  xTry $ grantForeignTransferRef ref dom
  writer $ "RCV: Writing reference (" ++ (show ref) ++ ").\n"
  put c ref
  writer "RCV: Finishing foreign transfer.\n"
  threadDelay $ 3 * 1000
  page <- forceTransfer ref
  writer "RCV: Completed transfer.\n"
  check <- isRightPageData page
  if check 
     then writer "RCV: Page transfer succeeded.\n"
     else writer "RCV: Page transferred incorrectly.\n"
 where
   forceTransfer :: GrantRef -> IO (VPtr a)
   forceTransfer ref = do
     res <- xTry $ finishForeignTransferRef ref
     case res of
       Right page -> return page
       Left _ -> do
         writer $ "RCV: " ++ show res
         forceTransfer ref

   writer = case args of
              ["writer=console"] -> \ s -> do
                                      writeConsole s
                                      writeDebugConsole s
              _                  -> writeDebugConsole

