{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

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
    -- * The RingBufferable type class
    RingBufferable(..)

    -- * Front End ring buffers
  , FrontRingBufferable(..)
  , FrontRingBuffer
  , frbCreate, frbCreateWithEC, frbAttach, frbShutdown
  , frbDomId
  , frbTryRequest, frbTryRequestMany
  , frbTryRequestSomeOf
  , frbRequest, frbRequestMany
  , frbTryRequestAsync, frbTryRequestManyAsync
  , frbTryRequestSomeOfAsync
  , frbGetResponses
  , frbGetResponsesNB

    -- * Back End ring buffers
  , BackRingBufferable(..)
  , BackRingBuffer
  , brbCreate, brbAttach, brbShutdown, brbDomId
  ) where

import Hypervisor.Basics (Xen, DomId, Err(EINVAL), ignoreErrors, xThrow, xOnException)
import Hypervisor.Memory (GrantRef, VPtr, GrantHandle, allocPage,
                          allocRef, freePage, freeRef, grantAccess, mapGrant,
                          systemMB, systemWMB, unmapGrant)
import Hypervisor.Port (Port, allocPort, bindRemotePort, closePort,
                        sendOnPort, setPortHandler, unsetPortHandler)
import Control.Concurrent.MVar (putMVar, takeMVar, MVar,
                                newEmptyMVar, newMVar)
import Control.Monad (Monad(..), Functor(..), MonadPlus(..),
                      forM, forM_, guard, unless, when)
import Data.Bits(Bits (shiftL, shiftR, (.&.)))
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(pokeByteOff, peekByteOff))
import Data.Traversable (Traversable, sequenceA)
import qualified Data.Map as Map



-- ---------------------------------------------------------------------------
--
-- RingBufferable
--
-- ---------------------------------------------------------------------------

-- |The type class to use to define a ring buffer. The type class is
-- parameterized over three types; the type of a request, the type of
-- a response, and the type of an id to use in matching requests and
-- responses.
class Ord id => RingBufferable req resp id | req  -> resp id
                                           , resp -> req id where
  -- |Given a request, provide something that can be used as an id. This
  -- id will be used to match the request to its answering response.
  requestId  :: req  -> id

  -- |Given a response, provide something that can be used as an id. 
  responseId :: resp -> id

  -- |The maximum amount of memory, in bytes, used for an entry in the
  -- ring buffer; in other words: max(sizeof request, sizeof response).
  -- If this number is greater than around 4080 bytes, the ring buffer
  -- code will fail during creation. Implementations of this routine 
  -- should not rely on either of the arguments, as the passed values
  -- may be undefined.
  entrySize  :: req -> resp -> Word32


class RingBufferable req resp id =>
      FrontRingBufferable req resp id | req -> resp id, resp -> req id where
  -- |Write a request object to the given pointer
  pokeRequest  :: Ptr a -> req -> IO ()

  -- |Read a response object from the given pointer
  peekResponse :: Ptr a -> IO resp


class RingBufferable req resp id =>
      BackRingBufferable req resp id | req -> resp id, resp -> req id where
  -- |Read a request object from the given pointer.
  peekRequest  :: Ptr a -> IO req

  -- |Write a response object to the given pointer
  pokeResponse :: Ptr a -> resp -> IO ()


-- ---------------------------------------------------------------------------
--
-- General ring buffer, parameterized over state.
--
-- ---------------------------------------------------------------------------

data RingBuffer s = RingBuffer
  { rbStateMV  :: MVar (Maybe s)
  , rbOtherDom :: !DomId
  , rbGrant    :: Either GrantRef GrantHandle
  , rbPort     :: !Port
  }


-- | Run a handler with the unlocked ring buffer state.
withRBState :: RingBuffer s -> M s a -> IO (Maybe a)
withRBState frb k = do
  let var = rbStateMV frb
  mb  <- takeMVar var
  (res, mb') <- case mb of
    Nothing    -> return (Nothing, Nothing)
    Just state -> do 
      (res, state') <- runM k state
      return (res, Just state')
  putMVar var mb'
  return res


-- ---------------------------------------------------------------------------
--
-- State Handler Monad
--
-- ---------------------------------------------------------------------------

newtype M i a = M { runM :: i -> IO (Maybe a,i) }

instance Functor (M i) where
  fmap f m = M (\ i -> do 
                 (r, i') <- runM m i
                 return (f `fmap` r, i'))

instance Monad (M i) where
  return a = M (\ i -> return (Just a,i))

  m >>= f  = M $ \ i -> do
    (mb, i') <- runM m i
    case mb of
      Nothing     -> return (Nothing, i)
      Just x      -> runM (f x) i'

instance MonadPlus (M i) where
  mzero     = M (\ i -> return (Nothing, i))

  mplus a b = M $ \ i -> do
    r@(mb, _) <- runM a i
    case mb of
      Nothing -> runM b i
      Just _  -> return r

class Monad m => HasIO m where
  io :: IO a -> m a

instance HasIO IO where
  io = id

instance HasIO (M i) where
  io m = M (\ i -> let f a = (Just a,i) in f `fmap` m)


get :: M i i
get  = M (\ i -> return (Just i,i))

gets :: (i -> a) -> M i a
gets f = M (\ i -> let a = f i in a `seq` return (Just a, i))

set :: i -> M i ()
set i = i `seq` M (\ _ -> return (Just (),i))

modify_ :: (i -> i) -> M i ()
modify_ f = M (\ i -> return (Just (),f i))

-- | Lookup the current space left, guarding based on a minimum value.
hasAtLeast :: Int -> M (FrontRBState rqt rpt idt) Int
hasAtLeast i = do
  state <- get
  let space = frbOpenSpace state
  guard (space >= i)
  return space

-- | Lookup the current space left, provided that there is some to lookup.
hasAvailSpace :: M (FrontRBState rqt rpt idt) Int
hasAvailSpace  = hasAtLeast 1

-- | Unwrap a maybe in IO, failing in the Nothing case.
unwrapMaybe :: String -> IO (Maybe a) -> IO a
unwrapMaybe msg m = do
  mb <- m
  case mb of
    Nothing -> fail msg
    Just a  -> return a


-- ---------------------------------------------------------------------------
--
-- Front End ring buffer types, code, and so forth
--
-- ---------------------------------------------------------------------------

type ResponseMap i rpt = Map.Map i (MVar rpt)

type FrontRingBuffer rqt rpt idt = RingBuffer (FrontRBState rqt rpt idt)

data FrontRBState rqt rpt idt = FrontRBState
  { frbReqProdPvt           :: !Word32
  , frbRespConsumed         :: !Word32
  , frbPage                 :: VPtr Word8
  , frbOpenSpace            :: !Int -- this is allowed to go negative
  , frbPendingRequests      :: [rqt] -- stored reversed (newest-first) 
  , frbResponseMap          :: ResponseMap idt rpt
  , frbUnhandledResponses   :: [rpt]
  , frbUnhandledRespWaiters :: [MVar Bool] -- stored reversed
  , frbNumEntries           :: !Word32
  , frbEntrySize            :: !Word32
  }


-- State Operations ------------------------------------------------------------

type Endo a = a -> a

-- | Apply a function to the response map of a frb state.
modifyResponseMap :: Endo (ResponseMap idt rpt)
                  -> Endo (FrontRBState rqt rpt idt)
modifyResponseMap f state = rm' `seq` state { frbResponseMap = rm' }
  where rm' = f (frbResponseMap state)


-- | Set the open space value in the frb state.
updateOpenSpace :: Int -> Endo (FrontRBState rqt rpt idt)
updateOpenSpace i state = state { frbOpenSpace = i }


-- | Update the number of responses consumed.
updateRespConsumed :: Word32 -> Endo (FrontRBState rqt rpt idt)
updateRespConsumed rc state = state { frbRespConsumed = rc }


-- | Add an unhandled response to the list.
addUnhandledResponse :: FrontRingBufferable rqt rpt idt
                     => rpt -> Endo (FrontRBState rqt rpt idt)
addUnhandledResponse rpt state = us' `seq` state { frbUnhandledResponses = us' }
  where us' = rpt : frbUnhandledResponses state


-- | Set the unhandled waiters list
setUnhandledWaiters :: [MVar Bool] -> Endo (FrontRBState rqt rpt idt)
setUnhandledWaiters ws state = ws `seq` state { frbUnhandledRespWaiters = ws }


-- | Set the pending requests
setPendingRequests :: FrontRingBufferable rqt rpt idt
                   => [rqt] -> Endo (FrontRBState rqt rpt idt)
setPendingRequests ps state = ps `seq` state { frbPendingRequests = ps }


-- | Add a request to the pending list.
addPendingRequest :: FrontRingBufferable rqt rpt idt
                  => rqt -> Endo (FrontRBState rqt rpt idt)
addPendingRequest p state = ps `seq` state { frbPendingRequests = ps }
  where ps = p : frbPendingRequests state


-- | Set the request pivot point.
setReqProdPvt :: Word32 -> Endo (FrontRBState rqt rpt idt)
setReqProdPvt pvt state = state { frbReqProdPvt = pvt }


-- | Generate a new MVar, add it to the response queue, and return the modified
-- frb state and MVar.
waitForResponse :: M (FrontRBState rqt rpt idt) (IO Bool)
waitForResponse  = do
  waitMV <- io newEmptyMVar
  state  <- get
  let urws' = waitMV : frbUnhandledRespWaiters state
  set state { frbUnhandledRespWaiters = urws' }
  urws' `seq` return (takeMVar waitMV)


-- | Pull out the unhandled responses from a state.
clearUnhandledResponses :: M (FrontRBState rqt rpt idt) [rpt]
clearUnhandledResponses  = do
  state <- get
  set state { frbUnhandledResponses = [] }
  return (frbUnhandledResponses state)


-- Interface -------------------------------------------------------------------

-- Trick to get correct overloading on entrySize.  This could
-- be done much simpler with scoped type variables as in GHC-6.6
checkEntrySize :: RingBufferable a b c
               => (Word32 -> Xen (RingBuffer (s a b c), GrantRef, Port))
               -> Xen (RingBuffer (s a b c), GrantRef, Port)
checkEntrySize f
  | s > maxEntrySize = xThrow EINVAL
  | otherwise        = f s
  where
  gs :: RingBufferable a b c
     => (Word32 -> Xen (RingBuffer (s a b c), GrantRef, Port)) -> a -> b
     -> Word32
  gs _ = entrySize
  s = gs f undefined undefined

-- |Create a new ring buffer front end to the given domain. This will return
-- the grant reference and event channel created along with the new ring 
-- buffer, which should simplify setup.
frbCreate :: (FrontRingBufferable rqt rpt idt)
          => DomId
          -> Xen ((FrontRingBuffer rqt rpt idt),GrantRef,Port)
frbCreate dom = checkEntrySize $ \ entSize -> do
  (page, ref, port) <- createCommChannel dom Nothing
  finishFRBCreate dom page ref port entSize

-- |Create a new FrontRBState.
emptyFrontRBState :: (RingBufferable rqt rpt idt)
                  => VPtr Word8 -> Word32 -> FrontRBState rqt rpt idt
emptyFrontRBState page entSize = FrontRBState
  { frbReqProdPvt           = 0
  , frbRespConsumed         = 0
  , frbPage                 = page
  , frbOpenSpace            = fromIntegral (numRingEntries entSize)
  , frbPendingRequests      = []
  , frbResponseMap          = Map.empty
  , frbUnhandledResponses   = []
  , frbUnhandledRespWaiters = []
  , frbNumEntries           = numRingEntries entSize
  , frbEntrySize            = entSize
  }

-- | Initialize the page used for the ring buffer.
initializePage :: VPtr Word8 -> IO ()
initializePage page = do
  setRingRequestsProduced  page 0
  setRingResponsesProduced page 0
  setRingRequestEvents     page 1
  setRingResponseEvents    page 1

-- |Just as frbCreate, but force the use of the given event channel. This is
-- only really useful if you're creating two ring buffers and want them to
-- share an event channel for scalability reasons.
frbCreateWithEC :: (FrontRingBufferable rqt rpt idt) => 
                   DomId -> Port -> 
                   Xen (FrontRingBuffer rqt rpt idt, GrantRef, Port)
frbCreateWithEC dom port = checkEntrySize $ \ entSize -> do
  (page, ref, p) <- createCommChannel dom (Just port)
  finishFRBCreate dom page ref p entSize

-- Do the common finishing code for a front-end ring buffer
finishFRBCreate :: (FrontRingBufferable rqt rpt idt)
                => DomId -> VPtr Word8 -> GrantRef -> Port -> Word32
                -> IO (FrontRingBuffer rqt rpt idt, GrantRef, Port)
finishFRBCreate dom page ref p entSize = do
  initializePage page
  stateMV    <- newMVar (Just (emptyFrontRBState page entSize))
  curHandler <- unsetPortHandler p
  setPortHandler p (frbInterrupt p stateMV >> curHandler)
  let frb = RingBuffer
        { rbStateMV  = stateMV
        , rbOtherDom = dom
        , rbPort     = p
        , rbGrant    = Left ref
        }
  return (frb, ref, p)

-- |Create a new front end by attaching it to an existing backend that has
-- provided us with a grant reference and event channel. Such situations
-- are pretty rare.
frbAttach :: (RingBufferable rqt rpt idt)
          => DomId -> GrantRef -> Port
          -> Xen (FrontRingBuffer rqt rpt idt)
frbAttach dom gref p = returnValues `fmap` checkEntrySize k
  where
  returnValues (x, _, _) = x

  k entSize = do
    (page, handle) <- mapGrant dom gref True
    p'  <- bindRemotePort dom p `xOnException` unmapGrant handle (Just page)
    stateMV <- newMVar (Just (emptyFrontRBState page entSize))
    let frb = RingBuffer
              { rbStateMV  = stateMV
              , rbOtherDom = dom
              , rbPort     = p'
              , rbGrant    = Right handle
              }
    return (frb, undefined, undefined)
 


-- |Shutdown a running ring buffer, disallowing any further access to it.
-- If the ring buffer is already shut down, this will silently do nothing,
-- so calling this routine multiple times on the same ring buffer should
-- be safe.
frbShutdown :: (RingBufferable rqt rpt idt)
            => FrontRingBuffer rqt rpt idt -> IO ()
frbShutdown ring = do
  let var = rbStateMV ring
  mb <- takeMVar var
  case mb of
    Nothing    -> return ()
    Just state -> ignoreErrors $ do
      let page = frbPage state
      closePort (rbPort ring)
      case rbGrant ring of
        Left gref     -> freeRef gref >> freePage page
        Right ghandle -> unmapGrant ghandle (Just page)
  putMVar var Nothing

-- |Get the domain id of the domain this front end is attached to.
frbDomId :: (RingBufferable rqt rpt idt)
         => FrontRingBuffer rqt rpt idt -> DomId
frbDomId = rbOtherDom

-- |Attempt to place a request on the ring buffer. If the ring is currently
-- full, this will immediately return Nothing. Otherwise, it will place the
-- request on the ring, wait for a response, and return that response.
frbTryRequest :: (FrontRingBufferable rqt rpt idt)
              => FrontRingBuffer rqt rpt idt -> rqt -> IO (Maybe rpt)
frbTryRequest ring req = sequenceA =<< body
  where
  body = withRBState ring $ do
    space <- hasAvailSpace
    resMV <- io newEmptyMVar
    modify_ $ modifyResponseMap (Map.insert (requestId req) resMV)
            . updateOpenSpace (space - 1)
    notify <- pushRequestsAndCheckNotify [req]
    return $ do
      when notify $ ignoreErrors $ sendOnPort $ rbPort ring
      takeMVar resMV

-- |Attempt to place a series of requests on the ring buffer. If the ring
-- is currently full or cannot support that many request, this will 
-- immediately return Nothing. Otherwise, it will place all of the requests
-- on the ring, wait for all the responses, and return them.
frbTryRequestMany :: (FrontRingBufferable rqt rpt idt)
                  => FrontRingBuffer rqt rpt idt -> [rqt] -> IO (Maybe [rpt])
frbTryRequestMany _ []      = return (Just [])
frbTryRequestMany ring reqs = sequenceA =<< body
  where
  body = withRBState ring $ do
    let numReqs = length reqs
    space    <- hasAtLeast numReqs
    respEnts <- io $ forM reqs $ \ x -> do
      mv <- newEmptyMVar
      return (requestId x, mv)
    modify_ $ modifyResponseMap (\rm -> Map.union rm (Map.fromList respEnts))
            . updateOpenSpace (space - numReqs)
    notify <- pushRequestsAndCheckNotify reqs
    return $ do
      when notify $ ignoreErrors $ sendOnPort $ rbPort ring
      mapM (takeMVar . snd) respEnts

-- |Attempt to place as many of the given requests on the ring buffer as
-- it can hold, and then wait for the responses. If the ring buffer is full,
-- this routine will immediately return the empty list. It is guaranteed that
-- the number of responses will be less than or equal to the number of 
-- requests.
frbTryRequestSomeOf :: (FrontRingBufferable rqt rpt idt)
                    => FrontRingBuffer rqt rpt idt -> [rqt] -> IO [rpt]
frbTryRequestSomeOf _    []   = return []
frbTryRequestSomeOf ring reqs = fromMaybe [] `fmap` (sequenceA =<< body)
  where
  body = withRBState ring $ do
    space <- hasAvailSpace
    let reqs' = take space reqs
    respEnts <- forM reqs' $ \ x -> do
      mv <- io newEmptyMVar
      return (requestId x, mv)
    modify_ $ modifyResponseMap (\rm -> Map.union rm (Map.fromList respEnts))
            . updateOpenSpace (space - length reqs')
    notify <- pushRequestsAndCheckNotify reqs'
    return $ do
      when notify $ ignoreErrors $ sendOnPort $ rbPort ring
      mapM (takeMVar . snd) respEnts

-- |Make a request to the backend and wait for a response. Unlike frbTryRequest,
-- this routine will block until there is sufficient space on the queue for the
-- request. Calling this routine on a closed ring buffer will trigger a failure.
frbRequest :: (FrontRingBufferable rqt rpt idt)
           => FrontRingBuffer rqt rpt idt -> rqt -> IO rpt
frbRequest ring req = unwrapMaybe err (sequenceA =<< body)
  where
  err  = "frbRequest: FRB request on closed ring buffer!"
  body = withRBState ring $ do
    resMV <- io newEmptyMVar
    space <- gets frbOpenSpace
    modify_ $ modifyResponseMap (Map.insert (requestId req) resMV)
            . updateOpenSpace (space - 1)

    let open = do
          guard (space > 0)
          notify <- pushRequestsAndCheckNotify [req]
          return $ do
            when notify $ ignoreErrors $ sendOnPort $ rbPort ring
            takeMVar resMV

    let noSpace = do
          modify_ (addPendingRequest req)
          return (takeMVar resMV)

    open `mplus` noSpace


-- |Send the given requests to the back end and return the back end's responses,
-- blocking if there isn't sufficient space on the ring to send the requests.
-- This routine is theoretically equivalent to repeatedly calling 
-- frbTryRequestSomeOf until every item as successfully been sent and all 
-- the responses received.
frbRequestMany :: (FrontRingBufferable rqt rpt idt)
               => FrontRingBuffer rqt rpt idt -> [rqt] -> IO [rpt]
frbRequestMany ring reqs = mapM (frbRequest ring) reqs

-- |Try to put the given request on the ring. If successful, immediately
-- returns success (True). If the ring is full, immediately returns False.
frbTryRequestAsync :: (FrontRingBufferable rqt rpt idt)
                   => FrontRingBuffer rqt rpt idt -> rqt -> IO Bool
frbTryRequestAsync ring req = fromMaybe False `fmap` (sequenceA =<< body)
  where
  body = withRBState ring $ do
    space <- hasAvailSpace
    modify_ (updateOpenSpace (space - 1))
    notify <- pushRequestsAndCheckNotify [req]
    return $ do
      when notify $ ignoreErrors $ sendOnPort $ rbPort ring
      return True

-- |Try to put the given requests on the ring. If successful, immediately
-- return success. If there is not enough space on the ring to put all the
-- requests, returns False.
frbTryRequestManyAsync :: (FrontRingBufferable rqt rpt idt)
                       => FrontRingBuffer rqt rpt idt -> [rqt] -> IO Bool
frbTryRequestManyAsync _    []   = return False
frbTryRequestManyAsync ring reqs = fromMaybe False `fmap` (sequenceA =<< body)
  where
  body = withRBState ring $ do
    let numReqs = length reqs
    space <- hasAtLeast numReqs
    modify_ (updateOpenSpace (space - numReqs))
    notify <- pushRequestsAndCheckNotify reqs
    return $ do
      when notify $ ignoreErrors $ sendOnPort $ rbPort ring
      return True

-- |Try to put as many of the given requests on the ring, returning the
-- number of requests successfully placed on the ring.
frbTryRequestSomeOfAsync :: (FrontRingBufferable rqt rpt idt)
                         => FrontRingBuffer rqt rpt idt -> [rqt] -> IO Int
frbTryRequestSomeOfAsync _    []   = return 0
frbTryRequestSomeOfAsync ring reqs = fromMaybe 0 `fmap` (sequenceA =<< body)
  where
  body = withRBState ring $ do
    space <- hasAvailSpace
    let reqs'   = take space reqs
    let numReqs = length reqs'
    modify_ (updateOpenSpace (space - numReqs))
    notify <- pushRequestsAndCheckNotify reqs'
    return $ do
      when notify $ ignoreErrors $ sendOnPort $ rbPort ring
      return numReqs

-- |Get some responses from the ring buffer, blocking until there are
-- available responses if there are none. For non-blocking behavior,
-- see frbGetResponsesNB. This routine is guaranteed to return the 
-- responses in the order in which they arrived; matching them against
-- the requests that generated them is the responsibility of the caller.
-- Calling this on a closed ring buffer will trigger a failure
frbGetResponses :: (RingBufferable rqt rpt idt)
                => FrontRingBuffer rqt rpt idt -> IO [rpt]
frbGetResponses ring = unwrapMaybe err (sequenceA =<< body)
  where
  err  = "frbGetResponses on a closed ring buffer!"
  body = withRBState ring $ do
    res <- clearUnhandledResponses
    if not (null res)
      then return (return res)
      else do
        waiter <- waitForResponse
        return $ do
          _ <- waiter
          frbGetResponses ring

-- |Exactly like frbGetResponses, except that it will return immediately
-- with the empty list if there are no responses available.
frbGetResponsesNB :: (FrontRingBufferable rqt rpt idt)
                  => FrontRingBuffer rqt rpt idt -> IO [rpt]
frbGetResponsesNB ring = fromMaybe [] `fmap` body
  where body = withRBState ring clearUnhandledResponses

-- The interrupt handler for a front ring buffer's event channel.
frbInterrupt :: (FrontRingBufferable rqt rpt idt)
             => Port -> MVar (Maybe (FrontRBState rqt rpt idt)) -> IO ()
frbInterrupt p stateMV = do
  mb  <- takeMVar stateMV
  mb' <- case mb of
    Nothing    -> return Nothing
    Just state -> (Just . snd) `fmap` runM frbInterrupt' state
  putMVar stateMV mb'
  where
  frbInterrupt' = do
    page       <- gets frbPage
    numEntries <- gets frbNumEntries
    entSize    <- gets frbEntrySize

    let basePtr = page `plusPtr` 64
    let eatNewResponses _    x        y        | x == y = return []
        eatNewResponses base consumed produced          = do
          let off = fromIntegral (consumed `mod` numEntries * entSize)
          response <- io (peekResponse (base `plusPtr` off))
          space    <- gets frbOpenSpace
          modify_ (updateOpenSpace (space + 1))
          rest     <- eatNewResponses base (consumed + 1) produced
          return ((responseId response, response):rest)

    resp_prod <- io (ringResponsesProduced page)
    resp_cons <- gets frbRespConsumed
    unless (resp_prod == resp_cons) $ do
      -- Get the new responses
      rsps <- eatNewResponses (castPtr basePtr) resp_cons resp_prod
      curUnhandleds <- gets frbUnhandledResponses
      -- Update the number of things we've consumed
      modify_ (updateRespConsumed resp_prod)
      -- Handle them all. This means putting their results in the 
      -- appropriate MVar if we have a handler for the id, or tacking
      -- them onto the unhandled list.
      forM_ rsps $ \ (eid, resp) -> do
        respMap <- gets frbResponseMap
        case Map.lookup eid respMap of
          Nothing   -> modify_ (addUnhandledResponse resp)
          Just mvar -> do
            io (putMVar mvar resp)
            modify_ (modifyResponseMap (Map.delete eid))
      newUnhandleds <- gets frbUnhandledResponses
      -- If there have been new unhandled items and there are waiters
      -- around waiting on us getting some, signal them.
      when (length curUnhandleds < length newUnhandleds) $ do
        waiters <- gets frbUnhandledRespWaiters
        let (is,l) = splitAt (length waiters - 1) waiters
        modify_ (setUnhandledWaiters is)
        case l of
          [w] -> io (putMVar w True)
          _   -> return ()
      -- If there are any pending requests and new open space, then
      -- write out the pending requests.
      pendings  <- gets frbPendingRequests
      openSpace <- gets frbOpenSpace
      let numPending = length pendings
      when (numPending > 0 && openSpace > 0) $ do
        let extraneous
              | openSpace >= numPending = 0
              | otherwise               = numPending - openSpace
        let newPendings = take extraneous pendings
        let newReqs     = reverse (drop extraneous pendings)
        modify_ (updateOpenSpace (openSpace - length newReqs))
        notify <- pushRequestsAndCheckNotify newReqs
        when notify $ io $ ignoreErrors $ sendOnPort p
        modify_ (setPendingRequests newPendings)
      -- See if there have been any new responses since we started
      goAgain <- finalCheckForResponses
      if goAgain 
         then frbInterrupt'
         else return ()
    where
    -- -------------------------------------------------------------
    finalCheckForResponses = do
      page <- gets frbPage
      respProd <- io (ringResponsesProduced page)
      respCons <- gets frbRespConsumed
      if respProd /= respCons
         then return True
         else do io (setRingResponseEvents page (respCons + 1))
                 io systemMB
                 respProd' <- io (ringResponsesProduced page)
                 respCons' <- gets frbRespConsumed
                 return (respProd' /= respCons')


-- Push a series of requests to the ring and check to see if we need to
-- send an event. 
-- INVARIANT #1: The ring must have sufficient space to hold
--               all the requests.
-- INVARIANT #2: There must be at least one request.
pushRequestsAndCheckNotify :: (FrontRingBufferable rqt rpt idt)
                           => [rqt] -> M (FrontRBState rqt rpt idt) Bool
pushRequestsAndCheckNotify [] = fail "INTERNAL ERROR (RB.pRACN)!"
pushRequestsAndCheckNotify requests = do
  entSize <- gets frbEntrySize
  page    <- gets frbPage
  forM_ requests $ \ x -> do
    prod       <- gets frbReqProdPvt
    numEntries <- gets frbNumEntries
    let off = 64 + fromIntegral ((prod `mod` numEntries) * entSize)
    let ptr = page `plusPtr` off
    io (pokeRequest ptr x)
    modify_ (setReqProdPvt (prod + 1))
  oldreqp <- io (ringRequestsProduced page)
  newreqp <- gets frbReqProdPvt
  reqevt  <- io $ do
    systemWMB
    setRingRequestsProduced page newreqp
    systemMB
    ringRequestEvents page
  return (newreqp - reqevt < newreqp - oldreqp)
  where


-- ---------------------------------------------------------------------------
--
-- Back End ring buffer types, code, and so forth
--
-- ---------------------------------------------------------------------------

type BackRingBuffer rqt rpt idt = RingBuffer (BackRBState rqt rpt idt)

data BackRBState rqt rpt idt = BackRBState
  { brbRespProdPvt :: !Word32
  , brbReqConsumed :: !Word32
  , brbPage        :: VPtr Word8
  , brbNumEntries  :: !Word32
  , brbEntrySize   :: !Word32
  }

setReqConsumed :: Word32 -> Endo (BackRBState rqt rpt idt)
setReqConsumed i state = state { brbReqConsumed = i }

setRespProdPvt :: Word32 -> Endo (BackRBState rqt rpt idt)
setRespProdPvt i state = state { brbRespProdPvt = i }


emptyBackRBState :: RingBufferable rqt rpt idt
                 => VPtr Word8 -> Word32 -> BackRBState rqt rpt idt
emptyBackRBState page entSize = BackRBState
  { brbRespProdPvt = 0
  , brbReqConsumed = 0
  , brbPage        = page
  , brbNumEntries  = numRingEntries entSize
  , brbEntrySize   = entSize
  }

-- |Create a back end ring buffer for the given domain, using the given 
-- function to convert requests into responses.
brbCreate :: (BackRingBufferable rqt rpt idt)
          => DomId -> (rqt -> IO rpt)
          -> Xen (BackRingBuffer rqt rpt idt, GrantRef, Port)
brbCreate dom responder = checkEntrySize $ \ entSize -> do
  (page, ref, p) <- createCommChannel dom Nothing

  initializePage page
  stateMV <- newMVar (Just (emptyBackRBState page entSize))
  setPortHandler p (brbInterrupt p responder stateMV)
  let brb = RingBuffer
            { rbStateMV  = stateMV
            , rbOtherDom = dom
            , rbPort     = p
            , rbGrant    = Left ref
            }
  return (brb, ref, p)

-- |Attach to an existing front end, again using the given function to 
-- convert requests into responses.
brbAttach :: (BackRingBufferable rqt rpt idt)
          => DomId -> GrantRef -> Port -> (rqt -> IO rpt)
          -> Xen (BackRingBuffer rqt rpt idt)
brbAttach dom gref p responder = returnValues `fmap` checkEntrySize k
  where
  returnValues (res,_,_) = res

  k entSize = do
    (page, handle) <- mapGrant dom gref True
    p' <- bindRemotePort dom p `xOnException` unmapGrant handle (Just page)
    stateMV <- newMVar (Just (emptyBackRBState page entSize))
    setPortHandler p' (brbInterrupt p' responder stateMV)
    let brb = RingBuffer
              { rbStateMV = stateMV
              , rbOtherDom = dom
              , rbPort = p'
              , rbGrant = Right handle
              }
    return (brb, undefined, undefined)


-- |Shutdown an existing back end. This can be safely called more than once
-- on the same ring buffer; all shutdowns after the first will do nothing.
brbShutdown :: (RingBufferable rqt rpt idt) =>
               BackRingBuffer rqt rpt idt -> IO ()
brbShutdown ring@RingBuffer { rbStateMV = stateMV } = do
  mb <- takeMVar stateMV
  case mb of
    Nothing    -> return ()
    Just state -> do
      let page = brbPage state
      closePort (rbPort ring)
      case rbGrant ring of
        Left gref     -> freeRef gref >> ignoreErrors (freePage page)
        Right ghandle -> ignoreErrors (unmapGrant ghandle (Just page))
  putMVar stateMV Nothing

-- |Get the domain ID of the domain the ring buffer is currently attached
-- to.
brbDomId :: (RingBufferable rqt rpt idt)
         => BackRingBuffer rqt rpt idt -> DomId
brbDomId = rbOtherDom

-- The interrupt handler for backend ring buffers
brbInterrupt :: (BackRingBufferable rqt rpt idt)
             => Port -> (rqt -> IO rpt)
             -> MVar (Maybe (BackRBState rqt rpt idt))
             -> IO ()
brbInterrupt p responder stateMV = do
  mb  <- takeMVar stateMV
  mb' <- case mb of
    Nothing    -> return Nothing
    Just state -> (Just . snd) `fmap` runM brbInterrupt' state
  putMVar stateMV mb'
 where
  brbInterrupt' = do
    state <- get
    let page    = brbPage state
    let basePtr = page `plusPtr` 64
    shouldRun <- unconsumedRequests
    when shouldRun $ do
      -- Get the new requests
      let reqs_cons = brbReqConsumed state
      reqs_prod <- io (ringRequestsProduced page)
      eatNewRequests (castPtr basePtr) reqs_cons reqs_prod
      -- It should be guaranteed that we always have enough space
      -- to put the response.
      notify <- pushResponsesAndCheckNotify
      when notify $ io $ ignoreErrors $ sendOnPort p
      goAgain <- finalCheckForRequests
      if goAgain
         then brbInterrupt'
         else return ()
   where
   -- --------------------------------------------------------------------
   unconsumedRequests = do
     state <- get
     let page = brbPage state
     -- This is pretty much a direct translation from xen/io/ring.h
     req_prod <- io (ringRequestsProduced page)
     let req_cons     = brbReqConsumed state
     let req          = req_prod - req_cons
     let rsp_prod_pvt = brbRespProdPvt state
     let num_ents     = brbNumEntries  state
     let rsp = num_ents - (req_cons - rsp_prod_pvt)
     return (min req rsp /= 0)
   -- --------------------------------------------------------------------
   finalCheckForRequests = do
     hasUncon <- unconsumedRequests
     if hasUncon
        then return True
        else do
          reqCons <- gets brbReqConsumed
          page    <- gets brbPage
          io $ do
            setRingRequestEvents page (reqCons + 1)
            systemMB
          unconsumedRequests
   -- --------------------------------------------------------------------
   eatNewRequests _    c        d        | c == d = return ()
   eatNewRequests base consumed produced          = do
     state <- get
     -- this is RING_REQUEST_CONS_OVERFLOW from xen/io/ring.h
     let rsp_prod_pvt = brbRespProdPvt state
     let numEntries   = brbNumEntries state
     if consumed - rsp_prod_pvt >= numEntries
        then return ()
        else do
          let entSize = brbEntrySize  state
          let off     = fromIntegral ((consumed `mod` numEntries) * entSize)
          req  <- io (peekRequest (base `plusPtr` off))
          resp <- io (responder req)
          writeElem resp
          let reqcon = brbReqConsumed state
          modify_ (setReqConsumed (reqcon + 1))
          eatNewRequests base (consumed + 1) produced
   -- --------------------------------------------------------------------
   writeElem x = do
     state <- get
     let prod       = brbRespProdPvt state
     let numEntries = brbNumEntries  state
     let entSize    = brbEntrySize   state
     let off = 64 + fromIntegral (prod `mod` numEntries * entSize)
     let ptr = brbPage state `plusPtr` off
     io (pokeResponse ptr x)
     modify_ (setRespProdPvt (prod + 1))

-- Push a series of responses to the ring and check to see if we need to
-- send an event. 
-- INVARIANT #1: The ring must have sufficient space to hold
--               all the requests.
-- INVARIANT #2: There must be at least one response
pushResponsesAndCheckNotify :: (RingBufferable rqt rpt idt)
                            => M (BackRBState rqt rpt idt) Bool
pushResponsesAndCheckNotify = do
  state <- get
  let page     = brbPage state
  let newrespp = brbRespProdPvt state
  oldrespp <- io (ringResponsesProduced page)
  respevt <- io $ do
    systemWMB
    setRingResponsesProduced page newrespp
    systemMB
    ringResponseEvents page
  return (newrespp - respevt < newrespp - oldrespp)

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

maxEntrySize :: Num a => a
maxEntrySize = 4080

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

ringResponseEvents :: Ptr a -> IO Word32
ringResponseEvents ptr = peekByteOff (castPtr ptr) 12

setRingResponseEvents :: Ptr a -> Word32 -> IO ()
setRingResponseEvents ptr val = pokeByteOff (castPtr ptr) 12 val

-- This is pretty much a direct translation of the __RD32 macro defined
-- in include/xen/io/ring.h. I thought it was kind of clever.
roundToPowerOfTwo :: Word32 -> Word32
roundToPowerOfTwo x | (x .&. 0xffff0000) /= 0 = 
  roundToPowerOfTwo (x `shiftR` 16) `shiftL` 16
roundToPowerOfTwo x | (x .&. 0x0000ff00) /= 0 =
  roundToPowerOfTwo (x `shiftR` 8) `shiftL` 8
roundToPowerOfTwo x | (x .&. 0x000000f0) /= 0 =
  roundToPowerOfTwo (x `shiftR` 4) `shiftL` 4
roundToPowerOfTwo x | (x .&. 0x0000000c) /= 0 =
  roundToPowerOfTwo (x `shiftR` 2) `shiftL` 2
roundToPowerOfTwo x | (x .&. 0x00000002) /= 0 = 2
roundToPowerOfTwo x = x .&. 1

-- Compute the number of entries on a ring based on the size of the 
-- entries. 
numRingEntries :: Word32 -> Word32
numRingEntries size = roundToPowerOfTwo ((4096 - 64) `div` size)

-- ---------------------------------------------------------------------------
--
-- Generally useful primitive options
--
-- ---------------------------------------------------------------------------

-- Create the page, grant reference and event channel required for a Xen
-- communication channel. This is just handy.
createCommChannel :: DomId -> Maybe Port -> 
                     Xen (VPtr Word8, GrantRef, Port)
createCommChannel dom Nothing = do
  p <- allocPort dom
  createCommChannel dom (Just p)
createCommChannel dom (Just p) = do
  page <- allocPage
  ref <- allocRef `xOnException` freePage page
  grantAccess ref dom page True
  return (page, ref, p)

