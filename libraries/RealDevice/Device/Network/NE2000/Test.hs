-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Driver.NE2000.Test where

import Driver.NE2000.Driver as NE2000
import Net.Test as Net(initialize)
import Dummy.IRQ(IRQ(..))

testnet putStrLn pci config =
  case pci of
    Nothing -> do eth <- NE2000.initialize putStrLn IRQ9 0x300
                  Just `fmap` start eth
    Just dev  -> do eth <- NE2000.initPCI putStrLn dev
                    case eth of
                      Nothing -> return Nothing
                      Just eth -> Just `fmap` start eth
  where
  start eth = Net.initialize putStrLn config eth

