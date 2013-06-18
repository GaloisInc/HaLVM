{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com> and Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- | Support for inter-domain communication through typed
-- communication channels.  The module provides types that represent
-- the input and ouput ends of open unidirectional communication
-- channels.  These channels are parameterized over the types of
-- messages that can be sent over them, so that domains can exchange
-- messages in a type-safe manner.
--
-- There are also bidirectional channels, parameterized over the types
-- of messages in each direction.
--
-- Given a communication channel, there are also operations for
-- obtaining a name for the channel.  Such names can be exchanged over
-- channels between domains.  This allows for dynamically creating new
-- channels between a set of domains, using existing channels.
--
-- High-level operations for establishing communication channels
-- between domains can be found in "RendezvousLib.PeerToPeer" and
-- "RendezvousLib.ClientServer".

module Communication.IVC (
    Channel(..)

  , Txt, Bin

  , InChannelEx, OutChannelEx, InOutChannelEx
  , InChannelNameEx, OutChannelNameEx, InOutChannelNameEx

  , InChannel, OutChannel, InOutChannel
  , InChannelName, OutChannelName, InOutChannelName

  , MakeChannelTo(..), AcceptChannel(..)
  , ChannelName(..), PageReference(..), mapPageReferences

  , PutChannel(..), putBinary
  , GetChannel(..), getBinary

  , PageCount

  -- * For internal use only
  , mkInChannelName
  , mkOutChannelName
  , mkInOutChannelName
  , marshall   -- Only for domain builder
  , unmarshall -- Only for glue code in domain children
  ) where

import Control.Monad
import Data.Serialize (Serialize,encode,decode)
import Data.Word
import Data.Generics.Basics(Data(..))
import Data.Generics.Aliases(ext1Q, ext1R)
import Data.Generics.MoreAliases(ext2Q, ext2R)
import Data.Generics.Instances()
import Data.Typeable(Typeable,gcast1)
import Data.ByteString.PText(pshow,pread', ReadP)
import Text.ReadP(readP_to_S)
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Basics
import Hypervisor.Memory
import Hypervisor.Port
import Util.WaitSet
import XenDevice.Xenbus(myDomId)

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe   as USBS

newtype PageCount = PageCount Word32
  deriving (Eq, Num, Ord, Show, Bounded, Enum, Real, Integral)

-- | IVC channels use shared memory pages for communication.
-- References to these pages can be passed between domains, either as
-- machine-frame numbers, or as grant references.

data PageReference =
   PR_GrantRef GrantRef
 | PR_MFN MFN
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- Ring buffer structure for these IVC channels:
--
-- +--------------------+ 0
-- |                    |
-- |        ...         |
-- |                    |
-- +--------------------+ bufferSize - 12
-- | Lock               |
-- +--------------------+ bufferSize - 8
-- | Bytes Consumed     |
-- +--------------------+ bufferSize - 4
-- | Bytes Produced     |
-- +--------------------+ bufferSize
--

-- Bidirectional channels share a page, and use the same structure as
-- unidirectional channels, each one using half a bufferSize.

rbOverhead :: Word32
rbOverhead = 12 -- for the two indexes and lock

rbConsumedOff :: Word32 -> Word32
rbConsumedOff size = size - 8

rbProducedOff :: Word32 -> Word32
rbProducedOff size = size - 4

rbLockedOff :: Word32 -> Word32
rbLockedOff size = size - 12

setConsumerOff :: VPtr a -> Word32 -> Word32 -> IO ()
setConsumerOff page size val =
  pokeByteOffW (castPtr page) (rbConsumedOff size) val

setProducerOff :: VPtr a -> Word32 -> Word32 -> IO ()
setProducerOff page size val =
  pokeByteOffW (castPtr page) (rbProducedOff size) val

consumerOff :: VPtr a -> Word32 -> IO Word32
consumerOff page size = peekByteOffW (castPtr page) (rbConsumedOff size)

producerOff :: VPtr a -> Word32 -> IO Word32
producerOff page size = peekByteOffW (castPtr page) (rbProducedOff size)

lockPage :: VPtr a -> Word32 -> IO ()
lockPage page size = spinlock (page `plusPtrW` rbLockedOff size)

unlockPage :: VPtr a -> Word32 -> IO ()
unlockPage page size = spinunlock (page `plusPtrW` rbLockedOff size)

-- Top-level operations

mkWriter' :: WaitSet -> (VPtr BS.ByteString,Word32) -> Port -> BS.ByteString
          -> IO ()
mkWriter' waitset (page,psize) prt = checkWrite
    where --checkWrite :: a -> IO ()
          checkWrite bstr = do
            let l = writerLen bstr
            -- Now see if we can just write some (or all) of it out right now.
            lockPage page psize
            prod <- producerOff page psize
            cons <- consumerOff page psize
	    unlockPage page psize
            case freeWriteSpace psize prod cons of
              -- There is no space left in the ring.
              x | x == 0 ->
                    stallWrite bstr
              -- There is space left in the ring, but it's less than the size
              -- of what we want to write out.
                | x < l ->
                    do let (start, end) = writerSplit x bstr
                       immediateWrite start prod (writerLen start)
                       -- it's possible that in the interim the other side has
                       -- read some more data, allowing for more free space
                       checkWrite end
              -- There is enough space left in the ring to write out this
              -- entire object.
                | otherwise ->
                    immediateWrite bstr prod l
          -- -------------------------------------------------------------------
          --stallWrite :: a -> IO ()
          stallWrite bstr = do
            ignoreErrors $ sendOnPort prt
            wait waitset $ do lockPage page psize
			      prod <- producerOff page psize
                              cons <- consumerOff page psize
			      unlockPage page psize
                              return $ freeWriteSpace psize prod cons /= 0
            checkWrite bstr
          -- -------------------------------------------------------------------
          --immediateWrite :: a -> Word32 -> Word32 -> IO ()
          immediateWrite bstr prod l = do
            -- index read must occur before data write across CPUs
            systemMB
	    lockPage page psize
            if (prod + fromIntegral l) > (psize - rbOverhead)
               then do let (start, end) =
			         writerSplit (psize - rbOverhead - prod) bstr
                       -- In this case, the write is wrapping around the buffer
                       copyByteString start (page `plusPtrW` prod)
                       copyByteString end (castPtr page)
                       systemWMB
                       setProducerOff page psize (writerLen end)
               else do copyByteString bstr (page `plusPtrW` prod)
                       systemWMB
                       setProducerOff page psize (prod + writerLen bstr)
	    unlockPage page psize
            -- data write must occur before index write across CPUs
            ignoreErrors $ sendOnPort prt

writerLen :: BS.ByteString -> Word32
writerLen = fromIntegral . BS.length

writerSplit :: Word32 -> BS.ByteString -> (BS.ByteString,BSI.ByteString)
writerSplit = BS.splitAt . fromIntegral

mkBSWriter :: WaitSet -> (VPtr BS.ByteString, Word32) ->
              Port -> BS.ByteString ->
              IO ()
mkBSWriter = mkWriter' {-(fromIntegral . BS.length)
                       (BS.splitAt . fromIntegral)
                       copyByteString-}

copyByteString :: BS.ByteString -> Ptr a -> IO ()
copyByteString s p = do
  USBS.unsafeUseAsCStringLen s (\(p',i) -> memcpy p p' (fromIntegral i))
  return ()

mkBSReader :: WaitSet -> (VPtr Word8, Word32) -> Port -> Word32 ->
              IO BS.ByteString
mkBSReader waitset ps prt size =
  BSI.create (fromIntegral size) (mkReader' waitset ps prt size)

mkReader' :: WaitSet -> (VPtr a,Word32) -> Port -> Word32 -> Ptr a -> IO ()
mkReader' waitset (page,psize) prt = checkRead
    where checkRead :: Word32-> Ptr a -> IO ()
          checkRead size buffer = do
	    lockPage page psize
            prod <- producerOff page psize
            cons <- consumerOff page psize
	    unlockPage page psize
            -- The amount of data available to be read is the number of bytes
            -- that have been produced but not consumed.
            case usedSpace psize prod cons of -- Word32 -> Int
              -- There is no data on the ring to be read
              x | x == 0 ->
                    stallRead size buffer
              -- There is data on the ring to read, but it's not as large as
              -- the amount we wanted to read
                | x < size ->
                    do immediateRead x cons buffer
                       -- The data might have appeared in the interim
                       checkRead (size - x) (buffer `plusPtrW` x)
              -- There is data on the ring to be read and it's at least as
              -- much as we want.
                | otherwise ->  -- aka:  x >= size ->
                    immediateRead size cons buffer
          -- ------------------------------------------------------------------
          stallRead :: Word32 -> Ptr a -> IO ()
          stallRead size buffer = do
            wait waitset $ do lockPage page psize
			      prod <- producerOff page psize
                              cons <- consumerOff page psize
			      unlockPage page psize
                              return $ usedSpace psize prod cons > 0
            checkRead size buffer
          -- ------------------------------------------------------------------
          immediateRead :: Word32 -> Word32 -> Ptr a -> IO ()
          immediateRead size cons buffer = do
            -- index read must occur before data read across CPUs
            lockPage page psize
            systemRMB
            let off = cons
            if (off + size) > (psize - rbOverhead)
               then do let size1 = psize - rbOverhead - cons
                           size2 = size - size1
                       -- In this case, the read is wrapping around the buffer
                       memcpy buffer (page `plusPtrW` cons) (fromIntegral size1)
                       memcpy (buffer `plusPtrW` size1) page (fromIntegral size2)
                       systemMB
                       setConsumerOff page psize size2
               else do memcpy buffer (page `plusPtrW` off) (fromIntegral size)
                       systemMB
                       setConsumerOff page psize (cons + size)
	    unlockPage page psize
            -- data read must occur before index write across CPUs
            ignoreErrors $ sendOnPort prt

freeWriteSpace :: Word32 -> Word32 -> Word32 -> Word32
freeWriteSpace psize prod cons
  | prod >= cons = psize - (prod - cons) - rbOverhead - 1
  | otherwise    = cons - prod  - 1 -- wraparound case

usedSpace :: Word32 -> Word32 -> Word32 -> Word32
usedSpace psize prod cons =
  psize - rbOverhead - freeWriteSpace psize prod cons - 1

plusPtrW :: Ptr a -> Word32 -> Ptr b
plusPtrW p x = p `plusPtr` (fromIntegral x)

peekByteOffW :: Storable a => Ptr a -> Word32 -> IO a
peekByteOffW p o = peekByteOff p (fromIntegral o)

pokeByteOffW :: Storable a => Ptr a -> Word32 -> a -> IO ()
pokeByteOffW p o x = pokeByteOff p (fromIntegral o) x

foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr b -> Word -> IO ()

foreign import ccall unsafe "spinlocks.h spinlock"
  spinlock :: Ptr a -> IO ()

foreign import ccall unsafe "spinlocks.h spinunlock"
  spinunlock :: Ptr a -> IO ()

-----------------------------------------------------
-- New interface

data Bin = Bin deriving (Data,Typeable)

data Txt = Txt deriving (Data,Typeable)


-- | The class of a channel types, parameterized over their message
-- type.  Contains operations that are common to both input channels
-- and output channels.

class Channel c where
  -- | Close the channel.  No further messages can be sent on it.
  close :: c -> IO ()
  -- | Get the page references associated with the channel.
  pageReference :: c -> [PageReference]
  -- | Get the port associated with the channel.
  port  :: c -> Port
  -- | Get the identity of the domain at the other end of the channel.
  peer :: c -> DomId

type OutChannel = OutChannelEx Txt

-- | Output channels for messages of type 'a'.
data OutChannelEx t a = OutChannel
   { oClose :: IO ()
   , oPageReference :: [PageReference]
   , oPort :: Port
   , oPeer :: DomId
   , rawWriter :: BS.ByteString -> IO ()
   }

instance Channel (OutChannelEx t a) where
  close = oClose
  pageReference = oPageReference
  port = oPort
  peer = oPeer

type InChannel = InChannelEx Txt

-- | Input channels for messages of type 'a'.
data InChannelEx t a = InChannel
   { iClose :: IO ()
   , iPageReference :: [PageReference]
   , iPort :: Port
   , iPeer :: DomId
   , rawReader :: Word32 -> IO BS.ByteString
   }

instance Channel (InChannelEx t a) where
  close = iClose
  pageReference = iPageReference
  port = iPort
  peer = iPeer

type InOutChannel = InOutChannelEx Txt

-- | Input-output channels: incoming messages have type 'i',
-- outgoing messages have type 'o'.
data InOutChannelEx t i o = InOutChannel
   { ioClose :: IO ()
   , ioPageReference :: [PageReference]
   , ioPort :: Port
   , ioPeer :: DomId
   , ioRawReader :: Word32 -> IO BS.ByteString
   , ioRawWriter :: BS.ByteString -> IO ()
   }

instance Channel (InOutChannelEx t i o) where
  close = ioClose
  pageReference = ioPageReference
  port = ioPort
  peer = ioPeer

-- | Name of any channel, suitable for passing between domains.

newtype CN = CN (DomId, [PageReference], Port)
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | Name for the input end of a channel, suitable for passing
-- between domains.
--
-- Note that the Data instance is bogus.  It cannot be used since we
-- want to keep channel names abstract.  Its only purpose is to allow
-- us to perform generic operations on data structures that contain
-- channel names inside this module.

type InChannelName = InChannelNameEx Txt

newtype InChannelNameEx t a = InChannelName CN
  deriving (Eq, Ord, Typeable)

-- | Name for the output end of a channel, suitable for passing
-- between domains.
--
-- Note that the Data instance is bogus.  It cannot be used since we
-- want to keep channel names abstract.  Its only purpose is to allow
-- us to perform generic operations on data structures that contain
-- channel names inside this module.

type OutChannelName = OutChannelNameEx Txt

newtype OutChannelNameEx t a = OutChannelName CN
  deriving (Eq, Ord, Typeable)

-- | Name for a bidirectional channel, suitable for passing
-- between domains.
--
-- Note that the Data instance is bogus.  It cannot be used since we
-- want to keep channel names abstract.  Its only purpose is to allow
-- us to perform generic operations on data structures that contain
-- channel names inside this module.

type InOutChannelName = InOutChannelNameEx Txt

newtype InOutChannelNameEx t i o = InOutChannelName CN
  deriving (Eq, Ord, Typeable)

-- | Create a channel to be offered to the given domain.

class MakeChannelTo c where
  makeChannelTo :: DomId -> PageCount -> IO c

instance MakeChannelTo (OutChannelEx t a) where
  makeChannelTo d s@(PageCount s') =
    do (g, p, pg, ws) <- makeChannelComponents d s
       let bytes = s' * 4096
       setProducerOff pg bytes 0
       setConsumerOff pg bytes 0
       return OutChannel{ oClose = undefined
                        , oPageReference = map PR_GrantRef g
                        , oPort = p
                        , oPeer = d
                        , rawWriter = mkBSWriter ws (pg,bytes) p
                        }

instance MakeChannelTo (InChannelEx t a) where
  makeChannelTo d s@(PageCount s') =
    do (g, p, pg, ws) <- makeChannelComponents d s
       let bytes = s' * 4096
       setProducerOff pg bytes 0
       setConsumerOff pg bytes 0
       return InChannel{ iClose = undefined
                       , iPageReference = map PR_GrantRef g
                       , iPort = p
                       , iPeer = d
                       , rawReader = mkBSReader ws (pg,bytes) p
                       }

instance MakeChannelTo (InOutChannelEx t i o) where
  makeChannelTo d s =
    do (g, p, pg, ws) <- makeChannelComponents d s
       makeIOChannel False d (map PR_GrantRef g) p pg ws s True

makeIOChannel :: Bool -> DomId -> [PageReference] -> Port ->
                 Ptr a -> WaitSet -> PageCount -> Bool ->
                 IO (InOutChannelEx t i o)
makeIOChannel acceptSide d g p pg ws (PageCount s) doInit =
  do let bytes          = 4096 * s
         readSize       = bytes `div` 2
         writeSize      = bytes - readSize
         pg2            = pg `plusPtrW` readSize
         (lower,higher) = ((pg, readSize), (pg2, writeSize))
         (rps,wps)      | acceptSide = (lower,higher)
                        | otherwise  = (higher,lower)
     when doInit $ do
       setProducerOff pg  readSize 0
       setConsumerOff pg  readSize 0
       setProducerOff pg2 writeSize 0
       setProducerOff pg2 writeSize 0
     return InOutChannel{ ioClose = undefined
                          , ioPageReference = g
                          , ioPort = p
                          , ioPeer = d
                          , ioRawReader = mkBSReader ws (castTuple rps) p
                          , ioRawWriter = mkBSWriter ws (castTuple wps) p
                          }
 where castTuple (a,b) = (castPtr a, b)

-- | Accept an offer to create a channel given a channel name.
class AcceptChannel cn c | cn -> c, c -> cn where
  acceptChannel :: cn -> IO c

instance AcceptChannel (InChannelNameEx t a) (InChannelEx t a) where
  acceptChannel (InChannelName (CN (d,g,p))) =
    do (ws, pg, p', s, doInit) <- acceptChannelComponents d g p
       let s' = 4096 * fromIntegral s
       when doInit $ do
         setProducerOff pg s' 0
         setConsumerOff pg s' 0
       return InChannel{ iClose = undefined
                       , iPageReference = g
                       , iPort = p'
                       , iPeer = d
                       , rawReader = mkBSReader ws (pg,s') p'
                       }

instance AcceptChannel (OutChannelNameEx t a) (OutChannelEx t a) where
  acceptChannel (OutChannelName (CN (d,g,p))) =
    do (ws, pg, p', s, doInit) <- acceptChannelComponents d g p
       let s' = 4096 * fromIntegral s
       when doInit $ do
         setProducerOff pg s' 0
         setConsumerOff pg s' 0
       return OutChannel{ oClose = undefined
                        , oPageReference = g
                        , oPort = p'
                        , oPeer = d
                        , rawWriter = mkBSWriter ws (pg,s') p'
                        }

instance AcceptChannel (InOutChannelNameEx t i o) (InOutChannelEx t i o) where
  acceptChannel (InOutChannelName (CN (d,g,p))) =
    do (ws, pg, p', s, doInit) <- acceptChannelComponents d g p
       makeIOChannel True d g p' pg ws s doInit

-- | These are bogus Data instances for channel names.  They cannot be
-- used since we want to keep channel names abstract.  Their only
-- purpose is to allow us to perform generic operations on data
-- structures that contain channel names.

instance Data a => Data (InChannelNameEx Txt a) where
  gunfold    = error "invalid use of InChannelName Data instance."
  toConstr   = error "invalid use of InChannelName Data instance."
  dataTypeOf = error "invalid use of InChannelName Data instance."
  dataCast1 f = gcast1 f

instance Data a => Data (OutChannelNameEx Txt a) where
  gunfold    = error "invalid use of OutChannelName Data instance."
  toConstr   = error "invalid use of OutChannelName Data instance."
  dataTypeOf = error "invalid use of OutChannelName Data instance."
  dataCast1 f = gcast1 f

instance (Data i, Data o) => Data (InOutChannelNameEx Txt i o) where
  gunfold    = error "invalid use of OutChannelName Data instance."
  toConstr   = error "invalid use of OutChannelName Data instance."
  dataTypeOf = error "invalid use of OutChannelName Data instance."
  dataCast1 f = gcast1 f

-- Private read and show "instances" for channel names

showInChannelName :: InChannelNameEx Txt a -> BS.ByteString
showInChannelName (InChannelName n) = marshall n

showOutChannelName :: OutChannelNameEx Txt a -> BS.ByteString
showOutChannelName (OutChannelName n) = marshall n

showInOutChannelName :: InOutChannelNameEx Txt i o -> BS.ByteString
showInOutChannelName (InOutChannelName n) = marshall n

readpInChannelName :: ReadP (InChannelNameEx Txt a)
readpInChannelName = InChannelName `fmap` unmarshallP

readpOutChannelName :: ReadP (OutChannelNameEx Txt a)
readpOutChannelName = OutChannelName `fmap` unmarshallP

readpInOutChannelName :: ReadP (InOutChannelNameEx Txt i o)
readpInOutChannelName = InOutChannelName `fmap` unmarshallP

-- Marshall/unmarshall messages

marshall :: Data a => a -> BS.ByteString
marshall = f
  where f :: Data a => a -> BS.ByteString
        f = pshow f `ext1Q` showInChannelName
                    `ext1Q` showOutChannelName
                    `ext2Q` showInOutChannelName

unmarshall :: forall a. Data a => BS.ByteString -> a
unmarshall x = case readP_to_S unmarshallP x of [(a, s)] | BS.null s -> a
                                                _ -> error ("cannot unmarshall "++show x++" into something of type "++show (dataTypeOf (undefined :: a)))

unmarshallP :: Data a' => ReadP a'
unmarshallP = pread' unmarshallP `ext1R` readpInChannelName
                                 `ext1R` readpOutChannelName
                                 `ext2R` readpInOutChannelName

-- | Output a message on a channel.

putBinary :: (Serialize a, PutChannel c a) => c -> a -> IO ()
putBinary  = put

class PutChannel c a | c -> a where
   put :: c -> a -> IO ()

instance Data a => PutChannel (OutChannelEx Txt a) a where
   put = putC

instance Serialize a => PutChannel (OutChannelEx Bin a) a where
   put = putB

instance Data o => PutChannel (InOutChannelEx Txt i o) o where
   put = putC

instance Serialize o => PutChannel (InOutChannelEx Bin i o) o where
   put = putB

-- | Read a message from a channel.

getBinary :: (Serialize a, GetChannel c a) => c -> IO a
getBinary  = get

class GetChannel c a | c -> a where
  get :: c -> IO a

instance Data a => GetChannel (InChannelEx Txt a) a where
  get = getC

instance Serialize a => GetChannel (InChannelEx Bin a) a where
  get = getB

instance Data i => GetChannel (InOutChannelEx Txt i o) i where
  get = getC

instance Serialize i => GetChannel (InOutChannelEx Bin i o) i where
  get = getB

-- Internal

liftError :: Either String a -> a
liftError (Left err) = error err
liftError (Right a)  = a

putC :: (Data a, PutRaw c) => c -> a -> IO ()
putC c a =
  do let b = marshall a
         len = fromIntegral (BS.length b) :: MessageSizeType
     putRaw c (encode len)
     putRaw c b

putB :: (Serialize a, PutRaw c) => c -> a -> IO ()
putB c a =
  do let b = encode a
         len = fromIntegral (BS.length b) :: MessageSizeType
     putRaw c (encode len)
     putRaw c b

getC :: (Data a, GetRaw c) => c -> IO a
getC c =
  do s <- getRaw c (fromIntegral $ sizeOf (undefined :: MessageSizeType))
     b <- getRaw c (fromIntegral (liftError (decode s) :: MessageSizeType))
     return (unmarshall b)

getB :: (Serialize a, GetRaw c) =>c -> IO a
getB c =
  do !s <- getRaw c (fromIntegral $ sizeOf (undefined :: MessageSizeType))
     -- writeDebugConsole $ "Start getRaw" ++ show s ++ "\n"
     let !size = fromIntegral (liftError (decode s) :: MessageSizeType)
     !b <- getRaw c size
     return (liftError (decode b))

-- | Low-level write operation for channels.  Writes a ByteString
-- to a channel.  Breaks type safety for channels.

class PutRaw c where
  putRaw ::  c -> BS.ByteString -> IO ()

instance PutRaw (OutChannelEx t a) where
  putRaw = rawWriter

instance PutRaw (InOutChannelEx t i o) where
  putRaw = ioRawWriter

-- | Low-level read operation for channels.  Reads a ByteString
-- from a channel.  One has to specify the size in
-- number of bytes to read.  Breaks type safety for channels.

class GetRaw c where
  getRaw ::  c -> Word32 -> IO BS.ByteString

instance GetRaw (InChannelEx t a) where
  getRaw = rawReader

instance GetRaw (InOutChannelEx t i o) where
  getRaw = ioRawReader

-- TODO: Rearrange channel types and operations, so that we don't need
-- to export these channel name constructors.

-- | Construct an 'InChannelName' from low-level stuff.  Only for use
-- in RendezvousLib.
mkInChannelName :: DomId -> [PageReference] -> Port -> InChannelNameEx t a
mkInChannelName d g p = InChannelName (CN (d,g,p))

-- | Construct an 'OutChannelName' from low-level stuff.  Only for use
-- in RendezvousLib.
mkOutChannelName :: DomId -> [PageReference] -> Port -> OutChannelNameEx t a
mkOutChannelName d g p = OutChannelName (CN (d,g,p))

-- | Construct an 'OutChannelName' from low-level stuff.  Only for use
-- in RendezvousLib.
mkInOutChannelName :: DomId -> [PageReference] -> Port
                   -> InOutChannelNameEx t a b
mkInOutChannelName d g p = InOutChannelName (CN (d,g,p))

-- | Construct a channel name from a channel.  Note that input
-- channels yield output-channel names and vice versa.  Also, the
-- channel name corresponding to a bidirectional channel will have its
-- input and output types swapped.

class ChannelName c n | c -> n, n -> c where
  channelName :: c -> IO n

-- | Given an input channel, get its output-channel name that can be
-- used in an accepting domain.
instance ChannelName (InChannelEx t a) (OutChannelNameEx t a) where
  channelName c = inChannel2Name c `fmap` myDomId

-- | Given an output channel, get its input-channel name that can be
-- used in an accepting domain.
instance ChannelName (OutChannelEx t a) (InChannelNameEx t a) where
  channelName c = outChannel2Name c `fmap` myDomId

-- | Given a bidirectional channel, get a bidirectional-channel name
-- that can be used in an accepting domain.  Note that the name will
-- have the input and output types swapped.
instance ChannelName (InOutChannelEx t i o) (InOutChannelNameEx t o i) where
  channelName c = inOutChannel2Name c `fmap` myDomId

-- Internal

inChannel2Name :: InChannelEx t a -> DomId -> OutChannelNameEx t a
inChannel2Name c me = OutChannelName $ CN (me, pageReference c, port c)

outChannel2Name :: OutChannelEx t a -> DomId -> InChannelNameEx t a
outChannel2Name c me = InChannelName $ CN (me, pageReference c, port c)

inOutChannel2Name :: InOutChannelEx t i o -> DomId -> InOutChannelNameEx t o i
inOutChannel2Name c me = InOutChannelName $ CN (me, pageReference c, port c)
type MessageSizeType = Word32

makeChannelComponents :: DomId -> PageCount ->
                         IO ([GrantRef], Port, VPtr a, WaitSet)
makeChannelComponents d s = do
  (page, grefs) <- initiateGrants d (fromIntegral s) True
  prt <- allocPort d
  waitSet <- newWaitSet
  setPortHandler prt (notify waitSet)
  return (grefs, prt, page, waitSet)

acceptChannelComponents :: DomId -> [PageReference] -> Port ->
                           IO (WaitSet, VPtr a, Port, PageCount, Bool)
acceptChannelComponents d prefs p = do
  -- XXX This is flaky if IVC is intradomain.
  iAmOfferer <- (==d) `fmap` myDomId
  page <- mapPageReferences (if iAmOfferer then domidSelf else d) prefs
  prt <- if iAmOfferer then return p else bindRemotePort d p
  waitSet <- newWaitSet
  setPortHandler prt (notify waitSet)
  return (waitSet, page, prt, fromIntegral $ length prefs, iAmOfferer)

-- | Map a page reference into our virtual address space.
mapPageReferences :: DomId -> [PageReference] -> IO (VPtr a)
mapPageReferences d ls
  | all isRef ls         = do (p, _) <- mapGrants d refs True
                              return p
  | all (not . isRef) ls = do p <- mapForeignMachineFrames d mfns
                              return p
  | otherwise            = fail "Gave inconsistent arguments"
 where
  isRef (PR_GrantRef _) = True
  isRef (PR_MFN _)      = False
  --
  refs = map (\ (PR_GrantRef g) -> g) ls
  mfns = map (\ (PR_MFN m) -> m) ls
