-- Copyright 2006-2008, Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |Support for inter-domain communication through typed
-- communication channels.  The module provides types that represent
-- the input and ouput ends of open unidirectional communication
-- channels.  These channels are parameterized over the types of
-- messages that can be sent over them, so that domains can exchange
-- messages in a type-safe manner.
--
-- There are also bidirectional channels, parameterized over the types
-- of messages in each direction.
--
module Communication.IVC(
         InChannel, OutChannel, InOutChannel
       , makeNewInChannel, acceptNewInChannel
       , makeNewOutChannel, acceptNewOutChannel
       , makeNewInOutChannel, acceptNewInOutChannel
       , get, put
       )
 where

import Communication.Rendezvous
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Binary hiding (get,put)
import Data.Binary.Put
import qualified Data.ByteString as BSS
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Memory
import Hypervisor.Port

data InChannel a = InChannel {
    ichSetupData :: Maybe (DomId, [GrantRef], Port)
  , ichInChannel :: InChan
  }

-- |Make a new input channel, targetting the given domain. The second argument
-- is the number of pages to use for the channel. (Note: the actual size of the
-- transfer buffer in memory will be slightly smaller than n * pageSize, because
-- some bookkeeping space is required)
makeNewInChannel :: Binary a => DomId -> Word -> IO (InChannel a)
makeNewInChannel target npages = do
  (grefs, port, ichn) <- makeNewChan target npages buildRawInChan
  return (InChannel (Just (target, grefs, port)) ichn)

-- |Accept a new input channel, given the input data.
acceptNewInChannel :: Binary a =>
                      DomId -> [GrantRef] -> Port ->
                      IO (InChannel a)
acceptNewInChannel target grants port = do
  ichn <- acceptNewChan target grants port buildRawInChan
  return (InChannel Nothing ichn)

data OutChannel a = OutChannel {
    ochSetupData  :: Maybe (DomId, [GrantRef], Port)
  , ochOutChannel :: OutChan
  }

-- |Make a new output channel, targetting the given domain. The second argument
-- is the number of pages to use for the channel. (Note: the actual size of the
-- transfer buffer in memory will be slightly smaller than n * pageSize, because
-- some bookkeeping space is required)
makeNewOutChannel :: Binary a =>
                     DomId -> Word ->
                     IO (OutChannel a)
makeNewOutChannel target npages = do
  (grefs, port, ochn) <- makeNewChan target npages buildRawOutChan
  return (OutChannel (Just (target, grefs, port)) ochn)

-- |Accept a new output channel, given the input data
acceptNewOutChannel :: Binary a =>
                       DomId -> [GrantRef] -> Port ->
                       IO (OutChannel a)
acceptNewOutChannel target grants port = do
  ochn <- acceptNewChan target grants port buildRawOutChan
  return (OutChannel Nothing ochn)

data InOutChannel a b = InOutChannel {
    bchSetupData  :: Maybe (DomId, [GrantRef], Port, Float)
  , bchInChannel  :: InChan
  , bchOutChannel :: OutChan
  }

-- |Make a new input / output channel targetting the given domain. The second
-- argument is the number of pages to use, while the third argument tells the
-- system what percentage to use for the input channel. This third argument
-- must be between 0 and 1, inclusive.
makeNewInOutChannel :: (Binary a, Binary b) =>
                       DomId -> Word -> Float ->
                       IO (InOutChannel a b)
makeNewInOutChannel target npages perc
  | (perc < 0) || (perc > 1.0) = throw EINVAL
  | otherwise                  = do
     (grs, p, (ich,och)) <- makeNewChan target npages (buildIOChan perc npages)
     return (InOutChannel (Just (target, grs, p, perc)) ich och)

-- |Accept a new input / out channel, given the input data
acceptNewInOutChannel :: (Binary a, Binary b) =>
                         DomId -> [GrantRef] -> Port -> Float ->
                         IO (InOutChannel a b)
acceptNewInOutChannel target grants port perc
  | (perc < 0) || (perc > 1.0) = throw EINVAL
  | otherwise                  = do
     let npages = fromIntegral (length grants)
     (ichn, ochn) <- acceptNewChan target grants port (buildIOChan perc npages)
     return (InOutChannel Nothing ichn ochn)

buildIOChan :: Float -> Word ->
               Bool -> Ptr Word8 -> Word -> Port ->
               IO (InChan, OutChan)
buildIOChan perc npages doClear ptr _ port = do
  let inSize  = floor ((fromIntegral (npages * 4096)) * perc)
      outSize = (npages * 4096) - inSize
  ichn <- buildRawInChan  doClear ptr                     inSize  port
  ochn <- buildRawOutChan doClear (ptr `plusPtrW` inSize) outSize port
  return (ichn, ochn)

makeNewChan :: DomId -> Word ->
               (Bool -> Ptr Word8 -> Word -> Port -> IO a) ->
               IO ([GrantRef], Port, a)
makeNewChan target npages buildChan = do
  ptr  <- mallocBytes (fromIntegral npages * 4096)
  refs <- grantAccess target ptr (fromIntegral npages * 4096) True
  port <- allocPort target
  ichn <- buildChan True ptr ((npages * 4096) - bookkeepingOverhead) port
  return (refs, port, ichn)

acceptNewChan :: DomId -> [GrantRef] -> Port ->
                 (Bool -> Ptr Word8 -> Word -> Port -> IO a) ->
                 IO a
acceptNewChan target grefs port buildChan = do
  myport <- bindRemotePort target port
  (ptr, _) <- mapGrants target grefs True
  let size = (length grefs * 4096) - bookkeepingOverhead
  buildChan False ptr (fromIntegral size) myport

-- -----------------------------------------------------------------------------

instance Binary a => RendezvousCapable Word (InChannel a) (OutChannel a) where
  makeConnection other size = do
    res <- makeNewOutChannel other size
    let Just (_, grs, ps) = ochSetupData res
    return (grs, [ps], return res)
  acceptConnection other refs [port] _ = acceptNewInChannel other refs port
  acceptConnection _ _ _ _ = fail "Should only have received one port!"

instance Binary a => RendezvousCapable Word (OutChannel a) (InChannel a) where
  makeConnection other size = do
    res <- makeNewInChannel other size
    let Just (_, grs, ps) = ichSetupData res
    return (grs, [ps], return res)
  acceptConnection other refs [port] _ = acceptNewOutChannel other refs port
  acceptConnection _ _ _ _ = fail "Should only have received one port!"

instance (Binary a, Binary b) =>
           RendezvousCapable (Float, Word) (InOutChannel a b) (InOutChannel b a)
 where
  makeConnection other (perc, size) = do
    res <- makeNewInOutChannel other size perc
    let Just (_, grs, ps, _) = bchSetupData res
    return (grs, [ps], return res)
  acceptConnection other refs [port] (perc, _) =
    acceptNewInOutChannel other refs port perc
  acceptConnection _ _ _ _ =
    fail "Should only have received one port!"

-- -----------------------------------------------------------------------------

class WriteableChan c a | c -> a where
  put :: c -> a -> IO ()

instance Binary a => WriteableChan (OutChannel a) a where
  put c = putBinary (ochOutChannel c)

instance Binary b => WriteableChan (InOutChannel a b) b where
  put c = putBinary (bchOutChannel c)

class ReadableChan c a | c -> a where
  get :: c -> IO a

instance Binary a => ReadableChan (InChannel a) a where
  get c = getBinary (ichInChannel c)

instance Binary a => ReadableChan (InOutChannel a b) a where
  get c = getBinary (bchInChannel c)

putBinary :: Binary a => OutChan -> a -> IO ()
putBinary oc x = runWriteRequest oc (encode x)

getBinary :: Binary a => InChan -> IO a
getBinary ic = decode `fmap` runReadRequest ic

-- -----------------------------------------------------------------------------

--
-- A communications channel is composed of something of a pair of a pointer
-- and a size, where:
--
--  +-----------------------+ ptr + 0
--  +         ...           |
--  +         ...           |
--  +     buffer space      |
--  +         ...           |
--  +         ...           |
--  +-----------------------+ ptr + size
--  +    bytes consumed     |
--  +-----------------------+ ptr + size + 4
--  +    bytes produced     |
--  +-----------------------+ ptr + size + 8
--

bytesConsumed :: Ptr Word8 -> Word -> IO Word32
bytesConsumed p s = peekByteOff (castPtr p) (fromIntegral s)

bytesProduced :: Ptr Word8 -> Word -> IO Word32
bytesProduced p s = peekByteOff (castPtr p) (fromIntegral s + 4)

setBytesConsumed :: Ptr Word8 -> Word -> Word32 -> IO ()
setBytesConsumed p s v = pokeByteOff (castPtr p) (fromIntegral s) v

setBytesProduced :: Ptr Word8 -> Word -> Word32 -> IO ()
setBytesProduced p s v = pokeByteOff (castPtr p) (fromIntegral s + 4) v

bookkeepingOverhead :: Integral a => a
bookkeepingOverhead = 8

-- Internal-only data structure
data OutChan = OutChan {
    ocBuffer  :: Ptr Word8
  , ocSize    :: Word
  , ocPort    :: Port
  , ocWaiting :: MVar [(ByteString, MVar ())]
  }

buildRawOutChan :: Bool -> Ptr Word8 -> Word -> Port -> IO OutChan
buildRawOutChan doClear buf size port = do
  when doClear $ bzero buf size
  waiters <- newMVar []
  let res = OutChan buf size port waiters
  setPortHandler port $ tryWriteData res
  return res

runWriteRequest :: OutChan -> ByteString -> IO ()
runWriteRequest och bs = do
  resMV <- newEmptyMVar
  modifyMVar_ (ocWaiting och) $ \ waiters ->
    return $! (msg, resMV) : waiters
  tryWriteData och
  takeMVar resMV
 where
  msg = encode (fromIntegral (BS.length bs) :: Word) `BS.append` bs

tryWriteData :: OutChan -> IO ()
tryWriteData och = modifyMVar_ (ocWaiting och) $ \ waiters -> do
  cons              <- bytesConsumed (ocBuffer och) (ocSize och)
  prod              <- bytesProduced (ocBuffer och) (ocSize och)
  (waiters', prod') <- doPossibleWrites prod cons waiters
  setBytesProduced (ocBuffer och) (ocSize och) prod'
  when (prod /= prod') $ sendOnPort (ocPort och)
  return waiters'
 where
  bufferSize = fromIntegral (ocSize och)
  --
  doPossibleWrites :: Word32 -> Word32 ->
                      [(ByteString, MVar())] ->
                      IO ([(ByteString, MVar())], Word32)
  doPossibleWrites prod _ [] = return ([], prod)
  doPossibleWrites prod cons ls@((bstr, resMV):rest) = do
    let mprod64 = fromIntegral prod :: Word64
        cons64  = fromIntegral cons :: Word64
        -- check for the rollover case
        prod64  = if mprod64 < cons64 then (mprod64 + 0x100000000) else mprod64
        bstrLn  = fromIntegral (BS.length bstr)
    case () of
      -- In this case, the buffer is full.
      () | prod64 - cons64 == bufferSize ->
        return (ls, prod)
      -- In this case, we have enough space to write the full bytestring.
      () | ((prod64 + bstrLn) - cons64) <= bufferSize -> do
        writeBS (ocBuffer och) (ocSize och) prod64 bstr
        putMVar resMV ()
        doPossibleWrites (prod + fromIntegral bstrLn) cons rest
      -- In this case, we have space to do a write, but not the whole
      -- bytestring
      () | otherwise -> do
        let room64  = fromIntegral (ocSize och) - (prod64 - cons64)
            (h,t)   = BS.splitAt (fromIntegral room64) bstr
        writeBS (ocBuffer och) (ocSize och) prod64 h
        return ((t, resMV) : rest, fromIntegral (prod64 + room64))

writeBS :: Ptr Word8 -> Word -> Word64 -> ByteString -> IO ()
writeBS buffer size logical_off lbstr =
  foldM_ doWrite logical_off (BS.toChunks lbstr)
 where
  doWrite :: Word64 -> BSS.ByteString -> IO Word64
  doWrite loff bstr = BSS.useAsCStringLen bstr $ \ (dptr, dlenI) -> do
    let real_off = fromIntegral (loff `mod` fromIntegral size)
        destPtr  = buffer `plusPtrW` real_off
        dlen     = fromIntegral dlenI
    if real_off + dlen > size
      then do let part1s = size - real_off
                  part2s = dlen - part1s
              memcpy destPtr dptr                     part1s
              memcpy buffer  (dptr `plusPtrW` part1s) part2s
      else    memcpy destPtr dptr dlen
    return (loff + fromIntegral dlen)

-- Internal-only data structure
data InChan = InChan {
    icBuffer    :: Ptr Word8
  , icSize      :: Word
  , icPort      :: Port
  , icStateMV   :: MVar InChanState
  }

data InChanState = NeedSize [MVar ByteString]
                 | GotSize Word ByteString [MVar ByteString]

buildRawInChan :: Bool -> Ptr Word8 -> Word -> Port -> IO InChan
buildRawInChan doClear buf size port = do
  when doClear $ bzero buf size
  stateMV <- newMVar (NeedSize [])
  let res = InChan buf size port stateMV
  setPortHandler port $ tryReadData res
  return res

runReadRequest :: InChan -> IO ByteString
runReadRequest ich = do
  resMV <- newEmptyMVar
  istate <- takeMVar (icStateMV ich)
  case istate of
    NeedSize waiters ->
      putMVar (icStateMV ich) $! NeedSize (waiters ++ [resMV])
    GotSize n acc waiters ->
      putMVar (icStateMV ich) $! GotSize n acc (waiters ++ [resMV])
  tryReadData ich
  takeMVar resMV

tryReadData :: InChan -> IO ()
tryReadData ich = modifyMVar_ (icStateMV ich) $ \ istate -> do
  prod             <- bytesProduced (icBuffer ich) (icSize ich)
  cons             <- bytesConsumed (icBuffer ich) (icSize ich)
  (istate', cons') <- doPossibleReads prod cons istate
  setBytesConsumed (icBuffer ich) (icSize ich) cons'
  when (cons /= cons') $ sendOnPort (icPort ich)
  return istate'
 where
  doPossibleReads :: Word32 -> Word32 -> InChanState -> IO (InChanState, Word32)
  doPossibleReads prod cons istate = do
    let mprod64 = fromIntegral prod :: Word64
        cons64  = fromIntegral cons :: Word64
        -- check for the rollover case
        prod64  = if mprod64 < cons64 then (mprod64 + 0x100000000) else mprod64
        avail   = fromIntegral (prod64 - cons64)
    case istate of
      -- If we need to get a size, we have waiters, and there's at least
      -- four bytes to read, then we should read off the size.
      NeedSize ws@(_:_) | prod64 >= (cons64 + sizeSize) -> do
        size <- decode `fmap` readBS (icBuffer ich) (icSize ich) cons64 sizeSize
        let istate' = GotSize size BS.empty ws
        doPossibleReads prod (fromIntegral (cons64 + sizeSize)) istate'
      -- If we have some data, but not enough, update ourselves with the
      -- new data and the lesser requirement.
      GotSize n acc ws | (avail > 0) && (n > avail) -> do
        part <- readBS (icBuffer ich) (icSize ich) cons64 avail
        let istate' = GotSize (n - avail) (acc `BS.append` part) ws
        doPossibleReads prod (cons + fromIntegral avail) istate'
      -- If we can read everything, do it!
      GotSize n acc (f:rest) | (avail > 0) && (n <= avail) -> do
        endp <- readBS (icBuffer ich) (icSize ich) cons64 n
        putMVar f (acc `BS.append` endp)
        doPossibleReads prod (cons + fromIntegral n) (NeedSize rest)
      -- Otherwise, we can't do anything
      _ ->
        return (istate, cons)

readBS :: Ptr Word8 -> Word -> Word64 -> Word -> IO ByteString
readBS buffer size logical_off amt = do
  let real_off = fromIntegral (logical_off `mod` fromIntegral size)
      readPtr  = buffer `plusPtrW` real_off
  if real_off + amt > size
    then do part1 <- packCStringLen readPtr (size - real_off)
            part2 <- packCStringLen buffer (amt - (size - real_off))
            return $! BS.fromStrict part1 `BS.append` BS.fromStrict part2
    else    BS.fromStrict `fmap` packCStringLen readPtr amt
 where
  packCStringLen :: Ptr Word8 -> Word -> IO BSS.ByteString
  packCStringLen p s = BSS.packCStringLen (castPtr p, fromIntegral s)

plusPtrW :: Ptr a -> Word -> Ptr a
plusPtrW p x = p `plusPtr` (fromIntegral x)

sizeSize :: Integral a => a
sizeSize = fromIntegral (BS.length (encode (0 :: Word)))

foreign import ccall unsafe "strings.h bzero"
  bzero :: Ptr a -> Word -> IO ()

foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr b -> Word -> IO ()
