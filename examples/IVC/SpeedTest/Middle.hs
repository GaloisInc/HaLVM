-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Communication.IVC
import Control.Monad
import Hypervisor.XenStore

import Common

main :: IO ()
main = do
  args <- alistArgs
  let Just inName      = lookup "inchan"  args
      (_, makeInChan)  = connectionBuilders inName
      Just outName     = lookup "outchan" args
      (makeOutChan, _) = connectionBuilders outName
  xs <- initXenStore
  ochan <- makeOutChan xs
  ichan <- makeInChan xs
  forever $ get ichan >>= put ochan
