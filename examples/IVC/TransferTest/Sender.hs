-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Common
import Control.Concurrent
import Hypervisor.Debug
import Hypervisor.DomainInfo
import Hypervisor.Memory
import Hypervisor.XenStore
import Communication.IVC

main :: IO ()
main = do
  writeDebugConsole "SND: Initializing XenStore.\n"
  xs <- initXenStore
  writeDebugConsole "SND: Waiting for accept.\n"
  c <- accept xs
  writeDebugConsole "SND: Waiting for reference.\n"
  ref <- get c
  writeDebugConsole ("SND: Making page. (Reference is " ++ show ref ++ ")\n")
  page <- makePageData
  writeDebugConsole ("SND: Transferring page " ++ show page ++ ".\n")
  transferFrame (peer c) ref page
  writeDebugConsole "SND: Page copied.\n"
  writeDebugConsole "SND: Done.\n"
  threadDelay 10000
