-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com> and Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Communication.IVC
import Communication.Rendezvous
import Control.Monad
import Hypervisor.Debug
import Hypervisor.XenStore

import Common

main :: IO ()
main = do
  writeDebugConsole "Sender start!\n"
  xs <- initXenStore
  args <- alistArgs
  writeDebugConsole ("ARGS: " ++ show args ++ "\n")
  let Just outName = lookup "outchan" args
  let (makeOutChan, _) = connectionBuilders outName
  writeDebugConsole "START: Building output channel.\n"
  ochan <- makeOutChan xs
  writeDebugConsole "START: Built output channel.\n"
  forever $ put ochan dataBlob
