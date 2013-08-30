module Common where

import Communication.IVC
import Communication.Rendezvous
import Data.Word
import Hypervisor.XenStore

leftSide  :: XenStore -> IO (InOutChannel Word Word)
rightSide :: XenStore -> IO (InOutChannel Word Word)
(leftSide, rightSide) = peerConnection "Doubler" (0.5, 1)

