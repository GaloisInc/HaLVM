module Common where

import Communication.IVC
import Communication.Rendezvous
import Data.ByteString.Lazy(ByteString)
import Hypervisor.XenStore

leftSide  :: XenStore -> IO (OutChannel ByteString)
rightSide :: XenStore -> IO (InChannel  ByteString)
(leftSide, rightSide) = peerConnection "PeerTest" 1

