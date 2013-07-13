module Common where

import Communication.IVC
import Communication.Rendezvous
import Hypervisor.XenStore

leftSide  :: XenStore -> IO (OutChannel Int)
rightSide :: XenStore -> IO (InChannel Int)
(leftSide, rightSide) = peerConnection "PeerTest" 2

