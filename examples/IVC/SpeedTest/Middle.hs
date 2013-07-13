{-# LANGUAGE ScopedTypeVariables #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Communication.IVC
import Control.Monad
import Data.ByteString.Lazy(ByteString)
import Data.List
import Hypervisor.Kernel
import RendezvousLib.PeerToPeer
import XenDevice.Xenbus

import Common

main :: IO ()
main = halvm_kernel [dXenbus] $ \ args -> do
  let args' = map killEqual $ map (break (== '=')) args
  case (lookup "inchan" args', lookup "outchan" args') of
    (Just inName, Just outName) -> do
      let (makeOutChan, _) = sizedP2PConnection chanSize outName
          (_, makeInChan)  = sizedP2PConnection chanSize inName
      ochan::(OutChannelEx Bin ByteString) <- makeOutChan
      ichan::(InChannelEx Bin ByteString)  <- makeInChan
      forever $ getBinary ichan >>= putBinary ochan
    (Just _, Nothing) -> fail "Could not parse out channel name!"
    (Nothing, Just _) -> fail "Could not parse in channel name!"
    (Nothing, Nothing) -> fail "Could not parse channel names!"
 where
  killEqual (a,'=':rest) = (a, rest)
  killEqual _            = error "No equal in second item!"

