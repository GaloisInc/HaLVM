-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- |Handy, singly-defined shortcuts for the various states used in the
-- Xen device connection protocol.
module XenDevice.XenRingState(xenRingInitialising,
                              xenRingInitialisingWait,
                              xenRingInitialised,
                              xenRingConnected,
                              xenRingClosing,
                              xenRingClosed)
    where

import Data.Word

-- |The ring is initialising
xenRingInitialising :: Word8
xenRingInitialising = 1

-- |The ring is waiting for the hot plug scripts to finish executing
xenRingInitialisingWait :: Word8
xenRingInitialisingWait = 2

-- |The ring is waiting for the other side to do something
xenRingInitialised :: Word8
xenRingInitialised = 3

-- |The ring is connected.
xenRingConnected :: Word8
xenRingConnected = 4

-- |The ring is about to close.
xenRingClosing :: Word8
xenRingClosing = 5

-- |The ring has been closed.
xenRingClosed :: Word8
xenRingClosed = 6
    
