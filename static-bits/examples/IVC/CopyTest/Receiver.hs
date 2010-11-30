-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Hypervisor.Debug
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
  writer "RCV: Creating destination page & granting access.\n"
  let dom = peer c
  ref <- allocRef
  page <- allocPage
  grantAccess ref dom page True
  writer "RCV: Writing reference.\n"
  put c ref
  writer "RCV: Waiting for completed transfer.\n"
  num <- waitForTransfer page 1
  writer $ "RCV: Page successfully copied after " ++ (show num) ++
                      " tries.\n"
 where 
   waitForTransfer :: VPtr a -> Integer -> IO Integer
   waitForTransfer page n = do
     res <- isRightPageData page
     if res 
       then return n
       else waitForTransfer page (n + 1)
 
   writer = case args of
              ["writer=console"] -> writeConsole
              _                  -> writeDebugConsole
