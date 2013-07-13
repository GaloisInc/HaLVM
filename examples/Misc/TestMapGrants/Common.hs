-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
module Common where

import Data.Word
import Foreign.Storable
import Hypervisor.Basics
import Communication.IVC
import Hypervisor.Memory
import RendezvousLib.PeerToPeer(P2PConnection(..))

offer  :: IO (OutChannel [GrantRef])
accept :: IO (InChannel [GrantRef])
(offer,accept) = p2pConnection "MapGrantsTest"

