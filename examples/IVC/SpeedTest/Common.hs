-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com> and Magnus Carlsson <magnus@galois.com>
-- BANNEREND
module Common(alistArgs, connectionBuilders, dataBlob) where

import Communication.IVC
import Communication.Rendezvous
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Hypervisor.XenStore
import System.Environment

chanSize :: Word
chanSize = 6

alistArgs :: IO [(String, String)]
alistArgs = (map killEqual . map splitEqual) `fmap` getArgs
 where
  killEqual (a,'=':rest) = (a, rest)
  splitEqual = break (== '=')

dataBlob :: ByteString
dataBlob = BS.take 4200 $ BS.pack bytelist
  where bytelist = [0..] ++ bytelist

type SendSideBuilder = XenStore -> IO (OutChannel ByteString)
type RecvSideBuilder = XenStore -> IO (InChannel ByteString)

connectionBuilders :: String -> (SendSideBuilder, RecvSideBuilder)
connectionBuilders n = peerConnection n chanSize

