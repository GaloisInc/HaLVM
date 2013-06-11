-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andrew Tolmach <apt@galois.com>, Adam Wick <awick@galois.com>
-- BANNEREND
{- Create channels that serve as arbitrary-sized buffers to raw I/O facilities with finite capacity. -}
-- |A helpful layering atop fixed-size buffers to make them appear to be infinite buffers.
module Hypervisor.BufferChan(
         mkReaderChan
       , mkWriterChan
       , mkBoundedReaderChan
       , mkBoundedWriterChan
       ) 
  where

import Control.Concurrent(forkIO)
import Control.Concurrent.BoundedChan(BoundedChan, newBoundedChan)
import qualified Control.Concurrent.BoundedChan as B
import Control.Concurrent.Chan(Chan, newChan)
import qualified Control.Concurrent.Chan as U
import Control.Monad(forever)
import Hypervisor.EventWaitSet
import Hypervisor.Port

generalMkReader :: IO (c a) -> (c a -> [a] -> IO ()) ->
                   IO [a] -> IO Bool -> EventWaitSet -> Port ->
                   IO (c a)
generalMkReader makeChannel writeList doRead canRead waitSet notifyPort = do
  readerChan <- makeChannel
  forkIO_ $ forever $ do
    wait waitSet canRead
    s <- doRead
    sendOnPort notifyPort
    writeList readerChan s
  return readerChan

-- |Make an unbounded channel for reading, given a computation for reading an
-- array of values, a computation determining whether we can read or not, an 
-- event wait set for blocking, and an event channel. Returns a Haskell channel
-- the caller can read from.
mkReaderChan :: IO [a] -> IO Bool -> EventWaitSet -> Port -> IO (Chan a)
mkReaderChan = generalMkReader newChan U.writeList2Chan 

-- |Make a bounded channel of length n for reading, given a computation for
-- reading an array of values, a computation determining whether we can read or
-- not, an event wait set for blocking, and an event channel. Returns a Haskell
-- channel the caller can read from.
mkBoundedReaderChan :: Int -> IO [a] -> IO Bool -> EventWaitSet -> Port -> 
                       IO (BoundedChan a)
mkBoundedReaderChan x = generalMkReader (newBoundedChan x) B.writeList2Chan 

generalMkWriter :: IO (c [a]) -> (c [a] -> IO [a]) ->
                   ([a] -> IO Int) -> IO Bool -> EventWaitSet -> Port -> 
                   IO (c [a])
generalMkWriter makeChannel readChan doWrite canWrite waitSet notifyPort = do
  writerChan <- makeChannel
  let outputThread leftovers = do
        -- This is made slightly more complicated than it should be by the
        -- fact that BoundedChan doesn't support unGetChan
        nexts <- case leftovers of
                   [] -> readChan writerChan
                   _  -> return leftovers
        wait waitSet canWrite
        i <- doWrite nexts
        sendOnPort notifyPort
        if (i < length nexts)
          then outputThread (drop i nexts)
          else outputThread []
  forkIO_ (outputThread [])
  return writerChan

-- |The expected counterpoint to mkReaderChan. Takes a function for writing
-- an array of values, a computation determining whether we can write or not,
-- an event wait set and an event channel. Returns a Haskell channel the
-- caller can write to.
mkWriterChan:: ([a] -> IO Int) -> IO Bool -> EventWaitSet -> Port -> 
               IO (Chan [a])
mkWriterChan = generalMkWriter newChan U.readChan

-- |The expected counterpoint to mkBoundedReaderChan. Takes a function for
-- writing an array of values, a computation determining whether we can write
-- or not, an event wait set and an event channel. Returns a Haskell channel
-- the caller can write to.
mkBoundedWriterChan :: Int -> 
                       ([a] -> IO Int) -> IO Bool -> EventWaitSet -> Port -> 
                       IO (BoundedChan [a])
mkBoundedWriterChan x = generalMkWriter (newBoundedChan x) B.readChan

forkIO_ :: IO () -> IO ()
forkIO_ m = forkIO m >> return ()
