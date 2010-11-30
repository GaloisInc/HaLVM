-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andrew Tolmach <apt@galois.com>
-- BANNEREND
{- An EventWaitSet represents a queue of processes waiting on predicates governed by a particular event port.
The assumption is that any change to the predicate will be followed by an event signal on that port.
-}
-- |A specialization of the general Util.WaitSet structures and routines for events.
module Hypervisor.EventWaitSet(
         EventWaitSet
       , newWaitSet
       , wait
       )
 where

import Hypervisor.Port
import qualified Util.WaitSet as WS

type EventWaitSet = WS.WaitSet

newWaitSet :: Port -> IO EventWaitSet
newWaitSet port =
   do ws <- WS.newWaitSet 
      setPortHandler port (WS.notify ws)
      return ws 

wait :: EventWaitSet -> IO Bool -> IO ()
wait = WS.wait
