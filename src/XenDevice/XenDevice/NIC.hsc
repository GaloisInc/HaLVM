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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module XenDevice.NIC (
         NIC
       , listNICs
       , openNIC
       , sendPacket
       , RxHandler
       , setReceiveHandler
       )
 where

import Communication.RingBuffer
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array (Array, listArray, (!), indices)
import Data.Bits
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy(ByteString,toChunks)
import Data.ByteString.Unsafe(unsafeUseAsCStringLen)
import Data.Int
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Word
import Foreign.ForeignPtr(ForeignPtr, withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr(mallocForeignPtrAlignedBytes)
import Hypervisor.Debug
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Memory
import Hypervisor.Port
import Hypervisor.XenStore

#include <stdint.h>
#include <stdlib.h>
#include "xen/xen.h"
#include "xen/io/netif.h"
#include "xen/io/xenbus.h"

data NIC = NIC {
    nicTxRing    :: !TxRing
  , nicTxId      :: !(MVar Word16)
  , nicTxTable   :: !(MVar (Map Word16 (MVar (Maybe ErrorCode), SBS.ByteString)))
  , nicRxHandler :: !(MVar RxHandler)
  }

listNICs :: XenStore -> IO [String]
listNICs xs = do
  ents <- xsDirectory xs "device/vif"
  forM (filter (not . null) ents) $ \ ent -> do
    xsRead xs ("device/vif/" ++ ent ++ "/mac")

openNIC :: XenStore -> String -> IO NIC
openNIC xs mac = do
  ents <- xsDirectory xs "device/vif"
  vifs <- forM (filter (not . null) ents) $ \ ent -> do
            backend <- xsRead xs ("device/vif/" ++ ent ++ "/backend")
            name <- xsRead xs ("device/vif/" ++ ent ++ "/mac")
            splits <- testFeature (backend ++ "/feature-split-event-channels")
            copy <- testFeature (backend ++ "/feature-rx-copy")
            if copy
              then return (name, ("device/vif/" ++ ent, backend, splits))
              else return ("", (undefined, undefined, undefined))
  let (feDir, beDir, splitECs) = fromMaybe (throw ENODEV) (lookup mac vifs)
  --
  dom <- read `fmap` xsRead xs (feDir ++ "/backend-id")
  let domid = toDomId (dom :: Word)
  txRing <- frbCreate nicTxRingType domid
  rxRing <- if splitECs
              then frbCreate       nicRxRingType domid
              else frbCreateWithEC nicRxRingType domid (frbGetPort txRing)
  xsWrite xs (feDir ++ "/tx-ring-ref") (show (unGrantRef (frbGetRef txRing)))
  xsWrite xs (feDir ++ "/rx-ring-ref") (show (unGrantRef (frbGetRef rxRing)))
  let txPort = frbGetPort txRing
      rxPort = frbGetPort rxRing
  if splitECs
    then do xsWrite xs (feDir ++ "/event-channel-tx") (show (fromPortW txPort))
            xsWrite xs (feDir ++ "/event-channel-rx") (show (fromPortW rxPort))
    else xsWrite xs (feDir ++ "/event-channel") (show (fromPortW txPort))
  xsWrite xs (feDir ++ "/request-rx-copy") "1"
  xsWrite xs (feDir ++ "/state") (show ((#const XenbusStateConnected)::Word))
  waitForState (beDir ++ "/state") (show ((#const XenbusStateConnected)::Word))
  xsWrite xs (feDir ++ "/state") (show ((#const XenbusStateConnected)::Word))
  --
  initRxTable <- newRxTable domid
  txIdMV <- newMVar 0
  txTableMV <- newMVar Map.empty
  rxHandlerMV <- newMVar (\ _ -> return ())
  --
  _ <- forkIO $ forever $ processTxResponses txRing txTableMV
  _ <- forkIO $ processRxResponses rxRing rxHandlerMV initRxTable
  return NIC {
    nicTxRing    = txRing
  , nicTxId      = txIdMV
  , nicTxTable   = txTableMV
  , nicRxHandler = rxHandlerMV
  }
 where
  fromPortW :: Port -> Word
  fromPortW = fromPort
  --
  testFeature key = handle (\ (_ :: ErrorCode) -> return False) $ do
    val <- xsRead xs key
    return (val == "1")
  --
  waitForState key val = do
    val' <- xsRead xs key
    unless (val' == val) $ do
      threadDelay 100000
      waitForState key val
  --
  processTxResponses txRing tableMV = do
    resps <- frbReadResponses txRing
    modifyMVar_ tableMV $ \ table -> do
      foldM handleTxResponse table resps
  handleTxResponse table resp =
    case Map.lookup (txrsId resp) table of
      Nothing -> do
        writeDebugConsole ("WARNING: Received response to unsent NIC tx req " ++ show (txrsId resp) ++ "\n")
        return table
      Just (retMV, _) -> do
        putMVar retMV (txrsStatus resp)
        return (Map.delete (txrsId resp) table)

sendPacket :: NIC -> ByteString -> IO ()
sendPacket nic bstr = do
  curId <- takeMVar (nicTxId nic)
  table <- takeMVar (nicTxTable nic)
  go True curId chunks [] [] table
 where
  odom    = frbGetDomain (nicTxRing nic)
  chunks  = toChunks bstr
  --
  go _ newId [] reqs mvars table = 
    do putMVar (nicTxId nic) $! newId
       putMVar (nicTxTable nic) $! table
       frbWriteRequests (nicTxRing nic) (reverse reqs)
       results <- mapM takeMVar mvars
       forM_ reqs $ \ (TxRequest ref _ _ _ _) -> endAccess ref
       processErrors results
  go isFirst newId (chunk : rest) reqs mvars table =
     unsafeUseAsCStringLen chunk $ \ (ptr, len) ->
       do -- If a chunk crosses a page boundary, then that's a problem for us
          let ptrW     = ptrToWordPtr ptr
              lenW     = fromIntegral len
              basePage = ptrW .&. (complement 4095)
              offset   = ptrW - basePage
              size     = min lenW (4096 - offset)
              crosses  = size < lenW
              chunk'   = if crosses
                            then [SBS.drop (fromIntegral size) chunk]
                            else []
              rest'    = chunk' ++ rest
          -- compute the values for the request
          let flags    = if null rest' then 0 else (#const NETTXF_more_data)
          [ref] <- grantAccess odom (wordPtrToPtr basePage) 4096 False
          mvar <- newEmptyMVar
          let size'    = if (flags > 0) && isFirst
                            then fromIntegral (BS.length bstr)
                            else size
              off16    = fromIntegral offset
              size16   = fromIntegral size'
              req      = TxRequest ref off16 flags newId size16
              table'   = Map.insert newId (mvar, chunk) table
          go False (newId + 1) rest' (req : reqs) (mvar : mvars) table'
  --
  processErrors []               = return ()
  processErrors (Nothing : rest) = processErrors rest
  processErrors (Just e  : _)    = throwIO e

-- Receive ---------------------------------------------------------------------

type RxTable = Array Word16 (GrantRef, ForeignPtr ())

-- | Generate a new RX table, containing 256 pages and grant references to the
-- given domain id.
newRxTable :: DomId -> IO RxTable
newRxTable domid =
  do entries <- replicateM 256 $
       do fptr  <- mallocForeignPtrAlignedBytes 4096 4096
          [ref] <- withForeignPtr fptr $ \ ptr ->
                     grantAccess domid ptr 4096 True
          return (ref,fptr)

     return (listArray (0,255) entries)

-- | Loop forever, processing events from the receive ring.
processRxResponses :: RxRing -> MVar RxHandler -> RxTable -> IO ()
processRxResponses rxRing handler table =
  do frbWriteRequests rxRing [ RxRequest i grant | i <- indices table
                                                 , let (grant,_) = table ! i ]
     go False []

  where

  -- the state we keep here is whether or not we're expecting an extra_info
  -- packet next, as well as the chunks that make up a partial packet.
  go extraInfo bstrs =
    do (reqs,pkts,extraInfo',bstrs') <-
         handleRxResponses table extraInfo bstrs =<< frbReadResponses rxRing

       -- handle all packets in a separate thread
       k <- readMVar handler
       _ <- forkIO (mapM_ k pkts)

       -- re-issue freed requests
       frbWriteRequests rxRing reqs

       go extraInfo' bstrs'


-- | Produce a set of packets and responses.
handleRxResponses :: RxTable -> Bool -> [SBS.ByteString] -> [RxResponse]
                  -> IO ([RxRequest],[ByteString],Bool,[SBS.ByteString])
handleRxResponses table = go [] []
  where

  -- case #1: we're expecting an extra_info, which we ignore.  Recover the
  -- resources associated with the original request.
  go reqs pkts True chunks (RxResponse { .. } : rest) =
    do let (grant,_) = table ! rxrsId
           req       = RxRequest rxrsId grant
       -- XXX: we need to process the fields of the netif_extra_info struct
       go (req:reqs) pkts False chunks rest

  -- case #2: this is real data
  go reqs pkts False chunks (RxResponse { .. } : rest)

    | rxrsStatus > 0 =
        withForeignPtr fp $ \ pg ->
          do let ptr = pg `plusPtr` fromIntegral rxrsOffset

             -- copy the relevant data out of the page
             bstr <- if rxrsStatus > 0
                        then SBS.packCStringLen (ptr,fromIntegral rxrsStatus)
                        else return SBS.empty

             if testBit rxrsFlags (#const _NETRXF_more_data)
                -- this was a partial packet, continue reading data
                then go reqs' pkts extraInfo (bstr:chunks) rest

                -- the packet is complete, start processing new ones
                else
                  do let pkt = BS.fromChunks (reverse (bstr : chunks))
                     go reqs' (pkt:pkts) extraInfo [] rest

    | otherwise =
      do writeDebugConsole $ "WARNING: Receive request error: "
                          ++ show rxrsStatus
                          ++ "\n"
         go reqs' pkts False chunks rest

    where

    (grant,fp) = table ! rxrsId
    req        = RxRequest rxrsId grant
    reqs'      = req : reqs
    extraInfo  = testBit rxrsFlags (#const _NETRXF_extra_info)

  -- all responses are processed
  go reqs pkts extraInfo chunks [] =
    return (reqs, reverse pkts, extraInfo, chunks)


type RxHandler = ByteString -> IO ()

-- | Install a handler for incoming packets.  There can only ever be one handler
-- installed; subsequent calls to setReceiveHandler will replace previous
-- handlers.
setReceiveHandler :: NIC -> RxHandler -> IO ()
setReceiveHandler nic handler = do
  _ <- takeMVar (nicRxHandler nic)
  putMVar (nicRxHandler nic) handler

-- ----------------------------------------------------------------------------

type TxRing = FrontEndRing TxRequest TxResponse

nicTxRingType :: FrontEndRingType TxRequest TxResponse
nicTxRingType  = FrontEndRingType {
    entrySize    = max (#size netif_tx_request_t) (#size netif_tx_response_t)
  , peekResponse = readTxResponse
  , pokeRequest  = writeTxRequest
  }

data TxResponse = TxResponse {
    txrsId     :: {-# UNPACK #-} !Word16
  , txrsStatus :: Maybe ErrorCode
  }
 deriving (Show)

readTxResponse :: Ptr TxResponse -> IO TxResponse
readTxResponse ptr = do
  i  <- (#peek netif_tx_response_t, id)     ptr
  st <- (#peek netif_tx_response_t, status) ptr
  case st :: Int16 of
    (#const NETIF_RSP_DROPPED) -> return $! TxResponse i Nothing
    (#const NETIF_RSP_ERROR)   -> return $! TxResponse i (Just EIO)
    (#const NETIF_RSP_OKAY)    -> return $! TxResponse i Nothing
    _                          -> return $! TxResponse i (Just EPROTO)

data TxRequest = TxRequest {
    txrqGrant  :: {-# UNPACK #-} !GrantRef
  , txrqOffset :: {-# UNPACK #-} !Word16
  , txrqFlags  :: {-# UNPACK #-} !Word16
  , txrqId     :: {-# UNPACK #-} !Word16
  , txrqSize   :: {-# UNPACK #-} !Word16
  }
 deriving (Show)

writeTxRequest :: Ptr TxRequest -> TxRequest -> IO ()
writeTxRequest ptr req = do
  (#poke netif_tx_request_t,gref)   ptr (unGrantRef (txrqGrant req))
  (#poke netif_tx_request_t,offset) ptr (txrqOffset req)
  (#poke netif_tx_request_t,flags)  ptr (txrqFlags req)
  (#poke netif_tx_request_t,id)     ptr (txrqId req)
  (#poke netif_tx_request_t,size)   ptr (txrqSize req)

-- ----------------------------------------------------------------------------

type RxRing = FrontEndRing RxRequest RxResponse

nicRxRingType :: FrontEndRingType RxRequest RxResponse
nicRxRingType  = FrontEndRingType {
    entrySize     = max (#size netif_rx_request_t) (#size netif_rx_response_t)
  , peekResponse  = readRxResponse
  , pokeRequest   = writeRxRequest
  }

data RxResponse = RxResponse {
    rxrsId     :: {-# UNPACK #-} !Word16
  , rxrsOffset :: {-# UNPACK #-} !Word16
  , rxrsFlags  :: {-# UNPACK #-} !Word16
  , rxrsStatus :: {-# UNPACK #-} !Int16
  }
 deriving (Show)

readRxResponse :: Ptr RxResponse -> IO RxResponse
readRxResponse ptr = do
  i <- (#peek netif_rx_response_t,id)     ptr
  o <- (#peek netif_rx_response_t,offset) ptr
  f <- (#peek netif_rx_response_t,flags)  ptr
  s <- (#peek netif_rx_response_t,status) ptr
  return $! RxResponse i o f s

data RxRequest = RxRequest {
    rxrqId    :: {-# UNPACK #-} !Word16
  , rxrqGrant :: {-# UNPACK #-} !GrantRef
  }

writeRxRequest :: Ptr RxRequest -> RxRequest -> IO ()
writeRxRequest ptr req = do
  (#poke netif_rx_request_t,id) ptr   (rxrqId req)
  (#poke netif_rx_request_t,gref) ptr (unGrantRef (rxrqGrant req))

-- foreign import ccall unsafe "memset"
--   memset :: Ptr a -> Word8 -> Word -> IO ()
