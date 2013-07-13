-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andrew Tolmach <apt@galois.com>
-- BANNEREND
-- |An implementation of a simple monitor protocol based on conditions.
module Util.WaitSet(
         WaitSet
       , newWaitSet
       , notify
       , wait
       )
 where

import Control.Concurrent.MVar

newtype WaitSet = WaitSet (MVar [MVar ()])

-- |Create a new wait set.
newWaitSet :: IO WaitSet
newWaitSet =
   do cond <- newMVar []
      return $ WaitSet cond

-- could be modified to store the predicates in the queue
-- and retest them before waking up corresponding threads
-- |Send a notify to all the items currently waiting.
notify :: WaitSet -> IO ()
notify (WaitSet cond) =
   do waiters <- takeMVar cond
      mapM_ (\b -> putMVar b ()) waiters
      putMVar cond []
      
-- |Wait on until a notify occurs and the given condition
-- holds.
wait :: WaitSet -> IO Bool -> IO ()
wait (WaitSet cond) p = loop 
 where 
  loop = 
   do waiters <- takeMVar cond
      b <- p    
      if b 
        then 
         do putMVar cond waiters
            return ()
        else 
         do block <- newEmptyMVar
            putMVar cond (block:waiters)
	    takeMVar block -- note that HALVM will never block if there is an event pending, so this is safe to use when notifying from an event handler
	    loop

