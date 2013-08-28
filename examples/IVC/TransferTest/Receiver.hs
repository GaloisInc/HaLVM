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
  writeDebugConsole "RCV: Creating destination reference.\n"
  r <- prepareTransfer (peer c)
  writeDebugConsole ("RCV: Telling other side about reference "++show r++".\n")
  put c r
  writeDebugConsole "RCV: Waiting for completed transfer.\n"
  mfn <- completeTransfer r True False
  writeDebugConsole "RCV: Mapping and checking page.\n"
  page <- mapFrames [mfn]
  isOK <- isRightPageData page
  if isOK
    then writeDebugConsole "RCV: Got the right page!\n"
    else writeDebugConsole "RCV: BAD BAD BAD WRONG PAGE RECEIVED!!!\n"

