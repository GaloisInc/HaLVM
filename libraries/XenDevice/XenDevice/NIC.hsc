-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- blank line for haddock
-- |Direct access to low-level network devices. You probably want to use some
-- higher-level interface instead.
--
-- Network devices are initialized using the same two-phase system as disks.
-- The initial boot process discovers a series of potential network devices.
-- Later, a program (or more likely, a network stack) may attempt to connect
-- to the network device. If successful, it can then use it to send and 
-- receive packets.
--

{-# OPTIONS_GHC -fno-warn-orphans #-}

module XenDevice.NIC (dNICs,
                      potentialNICs, notifyOnNewPotentialNIC,
                      NIC,
                      initializedNICs, notifyOnNewInitializedNIC,
                      isNICInitialized, initializeNIC,
                      getNICName,
                      RxHandler,
                      setReceiveHandler,
                      TxResult(..),
                      transmitPacket,
                      tryTransmitPacket)
    where

import XenDevice.Xenbus
import XenDevice.XenRingState

import Communication.RingBuffer
import Control.Concurrent
import Control.Exception(assert)
import Control.Monad
import Control.Monad.Error() -- For Monad (Either a) instance
import Data.Bits
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Internal as BSLI
import Data.Int
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.IOBase(unsafePerformIO)
import Hypervisor.Basics
import Hypervisor.Debug
import Hypervisor.Kernel
import Hypervisor.Memory

-- |The device driver data you should pass to halvm_kernel or
-- halvm_kernel_daemon
dNICs :: DeviceDriver
dNICs = DeviceDriver { devName = "XenDeviceInternalNICs"
                     , dependencies = [dXenbus]
                     , initialize = initNICDriver
                     , shutdown = shutdownNICDriver
                     }

-- ---------------------------------------------------------------------------
--
-- Device driver initialization and shutdown
--
-- ---------------------------------------------------------------------------

-- Initialize the network driver
initNICDriver :: IO ()
initNICDriver = do
  ret <- xsDirectory "device/vif"
  case ret of
    XBOk vif_devs -> mapM_ generatePotentialDevice vif_devs
    _ -> hDEBUG $ "VIF: No network devices found upon initial inspection.\n"
  ret' <- xsSetWatch "device/vif" onVIFModification
  case ret' of
    XBOk _ -> hDEBUG $ "VIF: Set new device watch on device/vif.\n"
    _ -> hDEBUG $ "VIF: Couldn't set new device watch on device/vif.\n"
 where onVIFModification :: WatchId -> String -> IO ()
       onVIFModification _ path =
           unless (any (\ x -> x == '/') (drop (length "device/vif") path))
                  (generatePotentialDevice (drop (length "device/vif") path))

-- Shut down the network driver
shutdownNICDriver :: IO ()
shutdownNICDriver = return () -- FIXME!

-- ---------------------------------------------------------------------------
--
-- Types, data structures and routines for potential NICs
--
-- ---------------------------------------------------------------------------

data PotentialNIC = PotentialNIC { potName              :: String
                                 , potHandle            :: Word16
                                 , potBackend           :: String
                                 , potBackendDom        :: DomId
                                 , potNodeName          :: String
                                 }

-- The set of potential NICs
potentials :: IORef [PotentialNIC]
potentials = unsafePerformIO $ newIORef []
{-# NOINLINE potentials #-}

-- The set of callbacks for newly-found potential devices
newPotentialCallbacks :: IORef [String -> IO ()]
newPotentialCallbacks = unsafePerformIO $ newIORef []
{-# NOINLINE newPotentialCallbacks #-}

potentialNICs :: IO [String]
potentialNICs = map potName `fmap` readIORef potentials

notifyOnNewPotentialNIC :: (String -> IO ()) -> IO ()
notifyOnNewPotentialNIC handler =
    atomicModifyIORef newPotentialCallbacks
       (\ ls -> (handler:ls, ()))

generatePotentialDevice :: String -> IO ()
generatePotentialDevice "" = return () -- Happens when we first set the watch
generatePotentialDevice _nodeName = do
  let nodeName = "device/vif/" ++ _nodeName
  _bidStr <- xsRead $ nodeName ++ "/backend-id"
  _macStr <- xsRead $ nodeName ++ "/mac"
  _backend <- xsRead $ nodeName ++ "/backend"
  case(_bidStr, _macStr, _backend) of
    (XBOk bidStr, XBOk macStr, XBOk backend) -> do
      let (virtDevId::Word16) = read _nodeName
          (backendId::Word16) = read bidStr
      hDEBUG $ "VIF: Found potential network device " ++ macStr ++ "\n"
      let newDevice = PotentialNIC { potName = macStr
                                   , potHandle = virtDevId
                                   , potBackend = backend
                                   , potBackendDom = (DomId backendId)
                                   , potNodeName = nodeName
                                   }
      runCallbacks newPotentialCallbacks macStr
      atomicModifyIORef potentials (\ ls -> (newDevice:ls, ()))
    _ -> hDEBUG $ "VIF: Failed to get XenStore info for " ++ nodeName ++ "\n"

-- ---------------------------------------------------------------------------
--
-- Types, data structures and routines for initialized NICs, including 
-- turning potential NICs into initialized NICs
--
-- ---------------------------------------------------------------------------

data NIC = NIC { initName               :: String
               , initTxRB               :: TxRingBuffer
               , initRxRB               :: RxRingBuffer
               , initBackendDom         :: DomId
               , initNextTxIdIO         :: IORef Word16
               , initReceiveProcIO      :: IORef RxHandler
               }

-- The set of initialized NICs
inittedNICs :: IORef [NIC]
inittedNICs = unsafePerformIO $ newIORef []
{-# NOINLINE inittedNICs #-}

-- The set of callbacks for NIC initialization
initializationCallbacks :: IORef [NIC -> IO ()]
initializationCallbacks = unsafePerformIO $ newIORef []
{-# NOINLINE initializationCallbacks #-}

-- |Returns the list of initialized devices.
initializedNICs :: IO [NIC]
initializedNICs = readIORef inittedNICs

-- |Register a callback that will be invoked every time a NIC is initialized.
notifyOnNewInitializedNIC :: (NIC -> IO ()) -> IO ()
notifyOnNewInitializedNIC handler =
    atomicModifyIORef initializationCallbacks
       (\ ls -> (handler:ls, ()))

-- |Get the name of a NIC. Currently, this is guaranteed to be the MAC 
-- address of the NIC.
getNICName :: NIC -> String
getNICName NIC{ initName = name } = name

-- |Returns True if the card with the given name (MAC address) has been
-- initialized.
isNICInitialized :: String -> IO Bool
isNICInitialized name =
  any (\ nic -> initName nic == name) `fmap` readIORef inittedNICs

-- |Attempt to initialize the NIC with the given MAC address. Returns the
-- initialized NIC upon success. Note that NICs can only be initialized once;
-- a successive call to this using the same MAC address will fail. The Maybe
-- argument is the size of the receive buffer in 4K pages. If left unset, this
-- is set to a standard default.
initializeNIC :: String -> Maybe Int -> IO (Maybe NIC)
initializeNIC name rxBufSize = do
  do res <- atomicModifyIORef potentials
               (\ ls ->
                    case partition (\ x -> (potName x) == name) ls of
                      ([], rest) -> (rest, Nothing)
                      ([item], rest) -> (rest, Just item)
                      _ -> error "INTERNAL ERROR: More than one NIC w/ name")
     case res of
       Just dev -> do asyncNotifMV <- newEmptyMVar
                      probeNIC dev asyncNotifMV rxBufSize
                      retval <- takeMVar asyncNotifMV
                      case retval of
                        Just idev -> do atomicModifyIORef inittedNICs
                                           (\ ls -> (idev:ls, ()))
                                        return retval
                        Nothing ->
                            return Nothing
       Nothing ->
           return Nothing

probeNIC :: PotentialNIC -> MVar (Maybe NIC) -> Maybe Int -> IO ()
probeNIC dev resMV rxBufSize = withRingBuffers finishProbe
    where withRingBuffers :: (TxRingBuffer -> RxRingBuffer ->
                              Word16 -> Word16 -> String ->
                              IO ())
                             -> IO ()
          withRingBuffers k = do
            (txrb, (GrantRef txGRef), port) <- frbCreate (potBackendDom dev) `catch` \e ->
                                                  fail $ "Couldn't create tx ring buffer: " ++ show e
         
            (rxrb, (GrantRef rxGRef), _) <- frbCreateWithEC (potBackendDom dev) port `catch` \e -> do
                                              frbShutdown txrb
                                              fail $ "Couldn't create rx ring buffer: " ++ show e

            k txrb rxrb txGRef rxGRef (drop 1 $ dropWhile (/= ' ') $ show port)
            
          -- ----------------------------------------------------------------
          finishProbe :: TxRingBuffer -> RxRingBuffer ->
                         Word16 -> Word16 -> String -> IO ()
          finishProbe txrb rxrb txGRef rxGRef port = do
            ret <- xsSetWatch (potBackend dev ++ "/state")
                              (onPossibleConnect txrb rxrb)
            case ret of
              XBOk _ -> do
                xsWrite (potNodeName dev ++ "/tx-ring-ref") (show txGRef)
                xsWrite (potNodeName dev ++ "/rx-ring-ref") (show rxGRef)
                xsWrite (potNodeName dev ++ "/event-channel") port
                xsWrite (potNodeName dev ++ "/request-rx-copy") "1"
                xsWrite (potNodeName dev ++ "/state") (show xenRingConnected)
                return ()
              _ -> do
                frbShutdown txrb
                frbShutdown rxrb
                fail $ "Couldn't set watch on " ++ potBackend dev ++ "\n"
          -- ----------------------------------------------------------------
          onPossibleConnect :: TxRingBuffer -> RxRingBuffer ->
                               WatchId -> String ->
                               IO ()
          onPossibleConnect txrb rxrb watch path = do
            _stateStr <- xsRead path
            case _stateStr of
              XBOk _s | read _s == xenRingInitialising -> return ()
                      | read _s == xenRingInitialisingWait -> return ()
                      | read _s == xenRingInitialised -> return ()
                      | read _s == xenRingConnected -> do
                xsUnsetWatch watch
                xsWrite (potNodeName dev ++ "/state") (show xenRingConnected)
                hDEBUG $ "VIF: Connected to device " ++ show (potName dev)++"\n"
                droppingReceiverIOR <- newIORef droppingReceiver
                nextIdIO <- newIORef 0
                let nic = NIC { initName = potName dev
                              , initTxRB = txrb
                              , initRxRB = rxrb
                              , initBackendDom = potBackendDom dev
                              , initNextTxIdIO = nextIdIO
                              , initReceiveProcIO = droppingReceiverIOR
                              }
                runCallbacks initializationCallbacks nic
                rxBuffer <- xTry $ allocRxBuffers (initBackendDom nic) rxBufSize
                case rxBuffer of
                  Right rxBuf -> do
                    forkIO $ startReceiving nic rxBuf
                    putMVar resMV $ Just nic
                  Left e -> do
                    hDEBUG $ "VIF: Couldn't allocate rx buffers for " ++
                             show (potName dev) ++ " (" ++ show e ++ ")\n"
                    putMVar resMV Nothing
              XBOk _s | (read _s) == xenRingClosing -> return ()
              XBOk _s | (read _s) == xenRingClosed -> return ()
              XBOk _s ->
                hDEBUG $ "VIF: Got weird ring state: " ++ show _s ++
                         " (ignoring)"
              XBError _s ->
                hDEBUG $ "VIF: Weird watch error: " ++ show _s ++
                         " (ignoring)"

-- ---------------------------------------------------------------------------
--
-- Types, data structures and routines for dealing with packet reception.
--
-- ---------------------------------------------------------------------------

type RxHandler = ByteString -> IO ()
data BufferBlock = BB !(VPtr Word8) !Word16

-- |Set the receive handler for the given NIC. All packets the NIC receives
-- will be passed back up, regardless of whether or not they're addressed
-- to us, so you'll have to manager filtering yourself.
setReceiveHandler :: NIC -> RxHandler -> IO ()
setReceiveHandler nic handler =
  atomicModifyIORef (initReceiveProcIO nic)
     (\ _ -> (handler, ()))

-- |Allocate the buffer space for packet reception. Also manages all the
-- grant reference work required for packet reception, so this may fail
-- due to a lack of memory space, a lack of available grant references,
-- or some other similar Xen error. The arguments are the domain ID of the
-- network backend and potentially a view describing how large of a buffer
-- to use (given in kilobytes; must be a multiple of 4). The return result
-- is a list of buffer blocks.
allocRxBuffers :: DomId -> Maybe Int -> Xen [BufferBlock]
allocRxBuffers dom Nothing          = allocRxBuffers dom $ Just 128
allocRxBuffers dom (Just rxBufSizeK)
 | rxBufSizeK `mod` 4 /= 0          = xThrow EINVAL
 | otherwise                        = do
   let rxBufPages = rxBufSizeK `div` 4
   pages <- allocN rxBufPages [] allocPage freePage
   refs  <- allocN rxBufPages [] allocRef freeRef `xOnException` mapM_ freePage pages
   zipWithM_ (\ p r -> grantAccess r dom p True) pages refs
   return $ zipWith (\ p (GrantRef g) -> BB p g) pages refs

allocN :: Int -> [a] -> Xen a -> (a -> Xen b) -> Xen [a]
allocN 0 accum _     _    = return accum
allocN n accum alloc free = do
    a <- alloc `xOnException` mapM_ free accum
    allocN (n-1) (a:accum) alloc free

startReceiving :: NIC -> [BufferBlock] -> IO ()
startReceiving _   []   = hDEBUG "VIF: FAILURE: No receive buffers!\n"
startReceiving nic bufs = rcv bufferIDs
 where
  reqHash   = Map.fromList [(i,p) | BB p i <- bufs]
  bufferIDs = map (\ (BB _ i) -> i) bufs
  handlerIO = initReceiveProcIO nic
  ring      = assert (length bufs == 128) $ initRxRB nic
  --
  rcv :: [Word16] -> IO ()
  -- If we ran out of buffers -- which should be rare -- then we should have
  -- some requests in flight. Thus, we block. If something has gone *really*
  -- wrong, then we've somehow lost track of our requests and this will block
  -- indefinitely; I don't know how to detect this.
  rcv []  = do resps    <- frbGetResponses ring
               done_ids <- processResponses [] resps []
               rcv done_ids
  rcv ids = do newlySent <- frbTryRequestSomeOfAsync ring ids
               let ids'  =  assert (newlySent > 0) $ drop newlySent ids
               resps     <- frbGetResponses ring
               done_ids  <- processResponses [] resps []
               rcv (done_ids ++ ids')
  --
  -- process responses from the other end.
  processResponses :: [BSI.ByteString] -> [RxResponse] -> [Word16] ->
                      IO [Word16]
  processResponses []     []    dones = return dones
  processResponses _      []    dones = do
       hDEBUG "VIF: Ended process responses with unfinished blocks!\n"
       return dones
  processResponses blocks (RxResponse i off flags status : rest) dones
    -- Error case
    | status < 0               = do
       hDEBUG $ "VIF: Error response from request: " ++ show status ++ "\n"
       processResponses [] rest (i:dones)
    -- Unspecificied case I'm not sure how to deal with.
    | status == 0              = do
       hDEBUG   "VIF: Received zero sized packet (error?) ignoring.\n"
       processResponses [] rest (i:dones)
    -- This packet has extra info we don't know how to deal with.
    | RxExtraInfo `elem` flags = do
       hDEBUG   "VIF: Ignoring packet with extra info.\n"
       processResponses [] rest (i:dones)
    -- This packet has more data coming next.
    | RxMoreData `elem` flags  = do
       let Just block = Map.lookup i reqHash
       prt <- BSI.create (fromIntegral status) $ \ ptr ->
                memcpy ptr (block `plusPtrW` off) $ fromIntegral status
       processResponses (prt:blocks) rest (i:dones)
    -- This packet is all here, or is finishing a previous packet
    | otherwise                = do
       let Just block = Map.lookup i reqHash
       prt <- BSI.create (fromIntegral status) $ \ ptr ->
                memcpy ptr (block `plusPtrW` off) $ fromIntegral status
       handler <- readIORef handlerIO
       handler $ BS.fromChunks $ reverse (prt:blocks)
       processResponses [] rest (i:dones)

droppingReceiver :: ByteString -> IO ()
droppingReceiver _ = return ()

-- ---------------------------------------------------------------------------
--
-- Types, data structures and routines for dealing with packet transmission.
--
-- ---------------------------------------------------------------------------

-- |A simple data type describing the result of a transmission attempt
data TxResult = TXOk -- ^The packet was successfully sent
              | TXPacketTooBig -- ^The packet was too large to be sent
              | TXBufferFull -- ^The buffer was full. This will only occur
                             -- using tryTransmitPacket
              | TXDropped -- ^The underlying network card dropped the packet
              | TXError -- ^There was some error in the network card
              | TXOutOfMemory -- ^The network driver ran out of memory or 
                              -- grant references trying to send the packet
  deriving (Eq,Show)

-- |Transmit a packet on the network. This routine will block until the
-- packet has been sent or the network card informs us of some error.
transmitPacket :: NIC -> ByteString -> IO TxResult
transmitPacket _ packet | BS.length packet > 4096 = return TXPacketTooBig
transmitPacket nic packet = do
  page <- xTry allocPage
  ref  <- xTry allocRef
  case (page, ref) of
    (Right p, Right r) -> do
      grantAccess r (initBackendDom nic) p False
      ident <- atomicModifyIORef (initNextTxIdIO nic) (\ x -> (x + 1, x))
      pokeBS p packet
      let size = fromIntegral $ BS.length packet
      let req  = TxRequest r 0 [] ident size
      resp <- frbRequest (initTxRB nic) req
      endAccess r >> freeRef r >> freePage p
      case txrsStatus resp of
        0  -> return TXOk
        -1 -> hDEBUG "TXError!\n" >> return TXError
        -2 -> hDEBUG "TXDropped!\n" >> return TXDropped
        _  -> error "XenDevice.NIC.transmitPacket, unhandled txrsStatus value"

    (Left _,  Left _)  -> do
      hDEBUG "VIF: TX: Both failed!\n"
      return TXOutOfMemory

    (Left _,  Right _) -> do
      hDEBUG "VIF: TX: Page alloc failed!\n"
      return TXOutOfMemory

    (Right _, Left _)  -> do
      hDEBUG "VIF: TX: Ref alloc failed!\n" 
      return TXOutOfMemory
 where
  pokeBS dptr bstr = foldM pokeBS' dptr (BS.toChunks bstr)
  pokeBS' dptr bstr = do
    withForeignPtr fptr $ \ sptr -> memcpy dptr (sptr `plusPtr` off) lenW
    return (dptr `plusPtr` len)
    where
    (fptr, off, len) = BSI.toForeignPtr bstr
    lenW             = fromIntegral len

-- |Transmit a packet on the network, but abort if the ring buffer is full.
-- It is my intention to eventually turn this routine into a true nonblocking
-- function, but at the moment it may block.
tryTransmitPacket :: NIC -> ByteString -> IO TxResult
tryTransmitPacket _ packet | BS.length packet > 1500 = return TXPacketTooBig
tryTransmitPacket _ _ = undefined


-- ---------------------------------------------------------------------------
--
-- Types and routines for dealing with the Xen NIC ring buffers
--
-- ---------------------------------------------------------------------------

#ifndef __i386__
#define __i386__
#endif
#include <xen/xen.h>
#include <xen/io/netif.h>

type TxRingBuffer = FrontRingBuffer TxRequest TxResponse Word16

data TxRequest = TxRequest {
    txrqGrant  :: GrantRef
  , txrqOffset :: Word16
  , txrqFlags  :: [TxFlags]
  , txrqId     :: Word16
  , txrqSize   :: Word16
  }
 deriving (Show)

data TxResponse = TxResponse {
    txrsId     :: Word16
  , txrsStatus :: Int16
  }
 deriving (Show)

data TxFlags = TxChecksumBlank
             | TxDataValidated
             | TxMoreData
  deriving (Eq, Show)

instance RingBufferable TxRequest TxResponse Word16 where
  requestId     = txrqId
  responseId    = txrsId
  entrySize _ _ = max (#size netif_tx_request_t) (#size netif_tx_response_t)

instance FrontRingBufferable TxRequest TxResponse Word16 where
  peekResponse ptr = do
    rid <- (#peek netif_tx_response_t, id) ptr
    status <- (#peek netif_tx_response_t, status) ptr
    return TxResponse { txrsId = rid, txrsStatus = status }

  pokeRequest ptr val = do
    bzero ptr (#size netif_tx_request_t)
    let (GrantRef grefNum) = txrqGrant val
        txFlags = txrqFlags val -- shorthand
        csumFlag = if TxChecksumBlank `elem` txFlags
                      then (#const NETTXF_csum_blank)
                      else 0
        dataValFlag = if TxDataValidated `elem` txFlags
                         then (#const NETTXF_data_validated)
                         else 0
        moreDataFlag = if TxMoreData `elem` txFlags
                          then (#const NETTXF_more_data)
                          else 0
        (flags::Word16) = csumFlag .|. dataValFlag .|. moreDataFlag
    (#poke netif_tx_request_t, gref) ptr grefNum
    (#poke netif_tx_request_t, offset) ptr $ txrqOffset val
    (#poke netif_tx_request_t, flags) ptr flags
    (#poke netif_tx_request_t, id) ptr $ txrqId val
    (#poke netif_tx_request_t, size) ptr $ txrqSize val

-- Yes, that's correct. Word16 to avoid boxing.
type RxRingBuffer = FrontRingBuffer Word16 RxResponse Word16

data RxResponse = RxResponse
  { rxrsId     :: !Word16
  , rxrsOffset :: !Word16
  , rxrsFlags  :: [RxFlags]
  , rxrsStatus :: Int16
  }

data RxFlags
  = RxDataValidated
  | RxChecksumBlank
  | RxMoreData
  | RxExtraInfo
  deriving (Eq, Show)

instance RingBufferable Word16 RxResponse Word16 where
  requestId x   = x
  responseId    = rxrsId
  entrySize _ _ = max (#size netif_rx_request_t) (#size netif_rx_response_t)

instance FrontRingBufferable Word16 RxResponse Word16 where
  peekResponse ptr = do
    rid <- (#peek netif_rx_response_t, id) ptr
    off <- (#peek netif_rx_response_t, offset) ptr
    (flags::Word16) <- (#peek netif_rx_response_t, flags) ptr
    status <- (#peek netif_rx_response_t, status) ptr
    let dataVal = if flags `testBit` (#const _NETRXF_data_validated)
                     then [RxDataValidated]
                     else []
        csumBlank = if flags `testBit` (#const _NETRXF_csum_blank)
                       then [RxChecksumBlank]
                       else []
        moreData = if flags `testBit` (#const _NETRXF_more_data)
                      then [RxMoreData]
                      else []
        extraInfo = if flags `testBit` (#const _NETRXF_extra_info)
                       then [RxExtraInfo]
                       else []
        outFlags = dataVal ++ csumBlank ++ moreData ++ extraInfo
    return RxResponse { rxrsId = rid, rxrsOffset = off,
                        rxrsFlags = outFlags, rxrsStatus = status }

  pokeRequest ptr val = do
    (#poke netif_rx_request_t, id) ptr val
    (#poke netif_rx_request_t, gref) ptr ((fromIntegral val)::Word32)

-- ---------------------------------------------------------------------------
--
-- Miscellaneous useful helper things
--
-- ---------------------------------------------------------------------------

hDEBUG :: String -> IO ()
hDEBUG = writeDebugConsole

plusPtrW :: Integral a => Ptr b -> a -> Ptr b
plusPtrW ptr off = ptr `plusPtr` fromIntegral off

runCallbacks :: IORef [a -> IO ()] -> a -> IO ()
runCallbacks ref val =
    readIORef ref >>= mapM_ (\ f -> f val)

foreign import ccall unsafe "strings.h bzero"
  bzero :: Ptr a -> Word32 -> IO ()

foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr a -> Word32 -> IO ()
