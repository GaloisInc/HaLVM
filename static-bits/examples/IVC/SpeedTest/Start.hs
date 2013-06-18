{-# LANGUAGE ScopedTypeVariables #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com> and Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Communication.IVC
import Control.Monad
import Data.ByteString.Lazy(ByteString)
import Data.Serialize
import Hypervisor.Debug
import Hypervisor.Kernel
import RendezvousLib.PeerToPeer
import XenDevice.Xenbus
import Common
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = halvm_kernel [dXenbus] $ \ args -> do
  writeDebugConsole "Sender start!\n"
  let args' = map killEqual $ map (break (== '=')) args
  case lookup "outchan" args' of
    Just outName -> do
      let (makeOutChan, _) = sizedP2PConnection chanSize outName
      writeDebugConsole "START: Building output channel.\n"
      ochan::(OutChannelEx Bin ByteString) <- makeOutChan
      writeDebugConsole "START: Built output channel.\n"
      let refs = pageReference ochan
      writeDebugConsole $ "START: Channel has " ++ show (length refs) ++ " items.\n"
      forever $ putBinary ochan dataBlob
    Nothing -> 
      fail "START: Couldn't parse out channel!"
 where
  killEqual (a,'=':rest) = (a, rest)
  killEqual _            = error "No equal in second item!"

dataBlob :: ByteString
dataBlob = BS.take 4200 $ BS.pack bytelist
  where bytelist = [0..] ++ bytelist
