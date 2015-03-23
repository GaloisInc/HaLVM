-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |This module implements a generalized ring buffer system for HALVM internals
-- and kernels. The goal is to shorten the development time required when 
-- writing device drivers for Xen-standard devices, as well as to make 
-- developing new devices much simpler. Usually, the only thing required to
-- create a new device is to define the request and response data structures,
-- and a function to transform requests to responses.
--
-- The ring buffer interface distinguishes between device front ends 
-- (requesters) and back ends (responders). However, both "sides" of a ring
-- buffer share a common data type to allow easy communication. NOTE: This
-- library will only function correctly if the maximum of the request size
-- and response size is constant.
--
module Communication.RingBuffer (
    FrontEndRingType(..)
  , FrontEndRing
  , frbCreate
  , frbCreateWithEC
  , frbGetDomain
  , frbGetRef
  , frbGetPort
  , frbWriteRequests
  , frbReadResponses
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Sequence(Seq, (><), (<|), (|>), ViewL(..), viewl)
import qualified Data.Sequence as Seq
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Memory
import Hypervisor.Port

data FrontEndRingType reqt respt = FrontEndRingType {
    entrySize    :: {-# UNPACK #-} !Word32
  , pokeRequest  :: Ptr reqt -> reqt -> IO ()
  , peekResponse :: Ptr respt -> IO respt
  }

-- ---------------------------------------------------------------------------
--
-- Front-end Ring Buffer API
--
-- --------------------------------------------------------------------------

data FrontEndRing reqt respt = FrontRing {
    frbType       :: !(FrontEndRingType reqt respt)
  , frbGetDomain  :: {-# UNPACK #-} !DomId
  , frbGetRef     :: {-# UNPACK #-} !GrantRef
  , frbGetPort    :: {-# UNPACK #-} !Port
  , frbRingSize   :: {-# UNPACK #-} !Word32
  , frbBase       :: {-# UNPACK #-} !(Ptr Word8)
  , frbLock       :: !(MVar ())
  , frbWriteReqs  :: !(IORef (Seq (reqt, MVar ())))
  , frbReadReqs   :: !(IORef (Seq (MVar [respt])))
  , frbReqProdPvt :: !(IORef Word32)
  , frbRespCons   :: !(IORef Word32)
  }

-- |Create a new ring buffer front end to the given domain. This will return
-- the grant reference and event channel created along with the new ring
-- buffer, which should simplify setup.
frbCreate :: FrontEndRingType a b -> DomId -> IO (FrontEndRing a b)
frbCreate t dom = allocPort dom >>= frbCreateWithEC t dom

-- |Just as frbCreate, but force the use of the given event channel. This is
-- only really useful if you're creating two ring buffers and want them to
-- share an event channel for scalability reasons.
frbCreateWithEC ::FrontEndRingType a b -> DomId -> Port -> IO (FrontEndRing a b)
frbCreateWithEC t dom port = do
  unless (entrySize t <= maxEntrySize) $ throw EINVAL
  page <- allocPage
  [ref] <- grantAccess dom page 4096 True
  setRingRequestsProduced  page 0
  setRingResponsesProduced page 0
  setRingRequestEvents     page 1
  setRingResponseEvents    page 1
  curHandler <- unsetPortHandler port
  lock <- newMVar ()
  wreqs <- newIORef Seq.empty
  rreqs <- newIORef Seq.empty
  prodRef <- newIORef 0
  consRef <- newIORef 0
  let retval = FrontRing {
         frbType = t
       , frbGetDomain = dom
       , frbGetRef = ref
       , frbGetPort = port
       , frbRingSize = roundedRingSize 1 ((4096 - 64) `div` entrySize t)
       , frbBase = page
       , frbLock = lock
       , frbWriteReqs = wreqs
       , frbReadReqs = rreqs
       , frbReqProdPvt = prodRef
       , frbRespCons = consRef
       }
  setPortHandler port (advanceFrontRingState retval >> curHandler)
  return retval
 where
  roundedRingSize x sz
    | (x `shiftL` 1) > sz = x
    | otherwise           = roundedRingSize (x `shiftL` 1) sz

-- |Write the given requests to the ring buffer. This function waits until
-- all the writes have been put on the ring before returning. Note that
-- getting the request on the ring doesn't necessarily mean that the back
-- end has read the request.
frbWriteRequests :: FrontEndRing a b -> [a] -> IO ()
frbWriteRequests ring reqs = do
  mvars <- mapM (const newEmptyMVar) reqs
  _ <- takeMVar (frbLock ring)
  modifyIORef' (frbWriteReqs ring) $ \ x -> x >< (Seq.fromList (zip reqs mvars))
  putMVar (frbLock ring) ()
  advanceFrontRingState ring
  mapM_ takeMVar mvars

-- |Read responses from the ring buffer. This routine is guaranteed to return
-- 1 or more responses, and will block until they are available.
frbReadResponses :: FrontEndRing a b -> IO [b]
frbReadResponses ring = do
  mvar <- newEmptyMVar
  _ <- takeMVar (frbLock ring)
  modifyIORef' (frbReadReqs ring) $ \ x -> x |> mvar
  putMVar (frbLock ring) ()
  advanceFrontRingState ring
  takeMVar mvar

-- Advance any state that's possible: write any requests we now have space for,
-- and read any responses that have shown up.
advanceFrontRingState :: FrontEndRing a b -> IO ()
advanceFrontRingState ring = do
  _ <- takeMVar (frbLock ring)
  -- process any responses we have available to us
  rreqs <- readIORef (frbReadReqs ring)
  respCons <- case viewl rreqs of
                EmptyL -> readIORef (frbRespCons ring)
                respMV :< rest -> do
                  (rc, resps) <- processResponses []
                  case resps of
                    [] -> return rc
                    _  -> do putMVar respMV resps
                             writeIORef (frbReadReqs ring) rest
                             return rc
  -- then process any write requests we can do
  reqProdPvt <- readIORef (frbReqProdPvt ring)
  let reqSpace = frbRingSize ring - (reqProdPvt - respCons)
  wreqs <- readIORef (frbWriteReqs ring)
  (wreqs', reqProdPvt') <- writeRequests reqSpace reqProdPvt (viewl wreqs)
  writeIORef (frbWriteReqs ring) wreqs'
  writeIORef (frbReqProdPvt ring) reqProdPvt'
  -- this is equivalent (hopefully) to RING_PUSH_REQUESTS_AND_CHECK_NOTIFY
  old <- ringRequestsProduced (frbBase ring)
  systemWMB
  setRingRequestsProduced (frbBase ring) reqProdPvt'
  systemMB
  reqEvt <- ringRequestEvents (frbBase ring)
  let writeWantsSignal = (reqProdPvt' - reqEvt) < (reqProdPvt' - old)
  when writeWantsSignal $ sendOnPort (frbGetPort ring)
  putMVar (frbLock ring) ()
 where
  processResponses oldResps = do
    rspProd <- ringResponsesProduced (frbBase ring)
    rspCons <- readIORef (frbRespCons ring)
    if rspProd > rspCons
      then do let unreadResponses = rspProd - rspCons
              (resps, rspCons') <- readResponses unreadResponses rspCons
              writeIORef (frbRespCons ring) rspCons'
              setRingResponseEvents (frbBase ring) (rspCons' + 1)
              systemMB
              processResponses (oldResps ++ resps)
      else return (rspCons, oldResps)
  --
  readResponses 0 rspCons = return ([], rspCons)
  readResponses n rspCons = do
    (rest, retval) <- readResponses (n - 1) (rspCons + 1)
    let idx = rspCons `mod` frbRingSize ring
        ptr = frbBase ring `plusPtrW` (64 + (idx * entrySize (frbType ring)))
    first <- peekResponse (frbType ring) (castPtr ptr)
    return (first : rest, retval)
  --
  writeRequests _ x EmptyL = return (Seq.empty, x)
  writeRequests 0 x (f :< rest) = return (f <| rest, x)
  writeRequests n prod ((first, firstMV) :< rest) = do
    let idx = prod `mod` frbRingSize ring
        ptr = frbBase ring `plusPtrW` (64 + (idx * entrySize (frbType ring)))
    pokeRequest (frbType ring) (castPtr ptr) first
    putMVar firstMV ()
    writeRequests (n - 1) (prod + 1) (viewl rest)
  --
  plusPtrW p w = plusPtr p (fromIntegral w)

-- ---------------------------------------------------------------------------
--
-- Ring buffer access and functions
--
-- ---------------------------------------------------------------------------

-- A ring buffer has the following format:
--
-- +---------------------------+ 0 
-- | # of requests produced    |
-- +---------------------------+ 4
-- | # of request events sent  |
-- +---------------------------+ 8
-- | # of responses produced   |
-- +---------------------------+ 12
-- | # of response events sent |
-- +---------------------------+ 16
-- |             ...           |
-- |           Padding         |
-- |             ...           |
-- +---------------------------| 64
-- |             ...           |
-- |             ...           |
-- |             ...           |
-- |          The Data         |
-- |             ...           |
-- |             ...           |
-- |             ...           |
-- +---------------------------| 4096

maxEntrySize :: Word32
maxEntrySize = 4096 - 64

ringRequestsProduced :: Ptr a -> IO Word32
ringRequestsProduced ptr = peekByteOff (castPtr ptr) 0

setRingRequestsProduced :: Ptr a -> Word32 -> IO ()
setRingRequestsProduced ptr val = pokeByteOff (castPtr ptr) 0 val

ringRequestEvents :: Ptr a -> IO Word32
ringRequestEvents ptr = peekByteOff (castPtr ptr) 4

setRingRequestEvents :: Ptr a -> Word32 -> IO ()
setRingRequestEvents ptr val = pokeByteOff (castPtr ptr) 4 val

ringResponsesProduced :: Ptr a -> IO Word32
ringResponsesProduced ptr = peekByteOff (castPtr ptr) 8

setRingResponsesProduced :: Ptr a -> Word32 -> IO ()
setRingResponsesProduced ptr val = pokeByteOff (castPtr ptr) 8 val

--ringResponseEvents :: Ptr a -> IO Word32
--ringResponseEvents ptr = peekByteOff (castPtr ptr) 12

setRingResponseEvents :: Ptr a -> Word32 -> IO ()
setRingResponseEvents ptr val = pokeByteOff (castPtr ptr) 12 val

