-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.PCI.Probe
  ( probe
  ) where

import Control.Monad(forM)
import Device.PCI.ConfigSpace
import Device.PCI.Device
import Data.Maybe(catMaybes)
import Data.Word

probe              :: IO DevTree
probe               = probeBus 0

-- Private ---------------------------------------------------------------------

probeBus           :: Word8 -> IO DevTree
probeBus b          = do devs <- concat `fmap` forM [0..31] (probeDev b)
                         loop devs (DevTree b [] [])
  where
  loop [] t         = return t
  loop (d:ds) t     = loop ds =<< tryDev d t

  tryDev d t        = do mb <- toPCIBridge d
                         case mb of
                           Nothing -> return (t { devices = d : devices t })
                           Just b'  -> 
                             do t' <- probeBus (secBus b')
                                return (t { bridges = (b',t') : bridges t})


probeDev           :: Word8 -> Word8 -> IO [Dev ()]
probeDev b dev      = do x <- probeConfig (configSpace b dev 0)
                         case x of
                           Nothing -> return []
                           Just d 
                             | isMultiFun (devInfo d) -> 
                                 ((d:) . catMaybes) `fmap`
                                 forM [1..7] (probeConfig . configSpace b dev)
                             | otherwise  -> return [d] 

probeConfig        :: ConfigSpace -> IO (Maybe (Dev ()))
probeConfig c       = pick `fmap` getDevInfo c
  where 
  pick d            
    | isPresent d   = Just (Dev { config  = c
                                , devInfo = d
                                , devData = ()
                                })
    | otherwise     = Nothing
                      


