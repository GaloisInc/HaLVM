-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Hypervisor.Debug
import Hypervisor.Memory
import Hypervisor.XenStore
import Hypervisor.Console
import Communication.IVC
import Common

main :: IO ()
main = do
  writeDebugConsole "RCV: Initializing XenStore.\n"
  xs <- initXenStore
  writeDebugConsole "RCV: Making offer.\n"
  c <- offer xs
  writeDebugConsole "RCV: Creating destination page & granting access.\n"
  page <- allocPage
  [r] <- grantAccess (peer c) page 4096 True
  writeDebugConsole ("RCV: Telling other side about reference "++show r++".\n")
  put c r
  writeDebugConsole "RCV: Waiting for completed transfer.\n"
  num <- waitForTransfer page 1
  writeDebugConsole $ "RCV: Page received after " ++ show num ++ " tries!\n"

waitForTransfer :: VPtr a -> Integer -> IO Integer
waitForTransfer page n = do
  res <- isRightPageData page
  if res
    then return n
    else waitForTransfer page (n + 1)

