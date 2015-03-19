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
module XenDevice.NIC (
         NIC
       , listNICs
       , openNIC
       , sendPacket
       , setReceiveHandler
       )
 where

import Communication.RingBuffer
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy(ByteString,fromChunks,toChunks)
import Data.ByteString.Unsafe(unsafePackCStringFinalizer, unsafeUseAsCStringLen)
import Data.Int
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Word
import Foreign.Ptr
import Foreign.Storable
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
    nicTxRing    :: FrontEndRing TxRequest TxResponse
  , nicTxId      :: MVar Word16
  , nicTxTable   :: MVar (Map Word16 (MVar (Maybe ErrorCode)))
  , nicRxHandler :: MVar (ByteString -> IO ())
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
  initRxTable <- foldM (buildRequest domid) Map.empty [0..255]
  txIdMV <- newMVar 0
  txTableMV <- newMVar Map.empty
  rxHandlerMV <- newMVar (\ _ -> return ())
  let reqs = Map.foldlWithKey (\ l i (g, _) -> RxRequest i g : l) [] initRxTable
  frbWriteRequests rxRing reqs
  --
  _ <- forkIO $ forever $ processTxResponses txRing txTableMV
  _ <- forkIO $ processRxResponses rxRing rxHandlerMV initRxTable False []
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
  buildRequest domid table i = do
    pg <- allocPage
    memset pg 0 4096
    [ref] <- grantAccess domid pg 4096 True
    return (Map.insert i (ref, pg) table)
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
      Just retMV -> do
        putMVar retMV (txrsStatus resp)
        return (Map.delete (txrsId resp) table)
  --
  processRxResponses rxRing handlerMV table st1 st2 = do
    rawresps <- frbReadResponses rxRing
    -- convert the raw responses into more useful items
    (ids, bstrs, table', st1', st2') <-
      convertRxResponses table st1 st2 rawresps
    --
    handler <- readMVar handlerMV
    forM_ bstrs $ \ x -> forkIO (handler x)
    --
    (reqs, table'') <- resendRequests (frbGetDomain rxRing) table' ids
    frbWriteRequests rxRing reqs
    processRxResponses rxRing handlerMV table'' st1' st2'
  --
  -- FIXME: What if we end with more data?
  convertRxResponses table st1 st2 [] = return ([], [], table, st1, st2)
  -- case #1: we're expecting an extra_info, which we don't handle
  convertRxResponses table True _ (_:rest) =
    convertRxResponses table False [] rest
  -- case #2: this is real data
  convertRxResponses table False acc (rresp:rest) = do
    let err = rxrsStatus rresp
    case Map.lookup (rxrsId rresp) table of
      Nothing -> do
        writeDebugConsole "WARNING: Received response to unsent NIC rx req.\n"
        convertRxResponses table False [] rest
      Just (grant, pg) | err > 0 -> do
        endAccess grant
        let ptr    = pg `plusPtr` fromIntegral (rxrsOffset rresp)
            len    = fromIntegral err
            table' = Map.delete (rxrsId rresp) table
        bstr <- unsafePackCStringFinalizer ptr len (freePage pg)
        if rxrsFlags rresp .&. (#const NETRXF_more_data) /= 0
          then convertRxResponses table' False (bstr : acc) rest
          else do (idr, bstrr, table'', s1, s2) <-
                    convertRxResponses table' False [] rest
                  let bstr' = fromChunks (bstr : reverse acc)
                  return (rxrsId rresp : idr, bstr' : bstrr, table'', s1, s2)
      Just (_, _) -> do
        writeDebugConsole ("WARNING: Receive request error: "++show err++"\n")
        convertRxResponses table False [] rest
  --
  resendRequests _   table [] = return ([], table)
  resendRequests dom table (i:rest) = do
    pg <- allocPage
    memset pg 0 4096
    [ref] <- grantAccess dom pg 4096 True
    let table' = Map.insert i (ref, pg) table
    (rest', table'') <- resendRequests dom table' rest
    return (RxRequest i ref : rest', table'')

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
              table'   = Map.insert newId mvar table
          go False (newId + 1) rest' (req : reqs) (mvar : mvars) table'
  --
  processErrors []               = return ()
  processErrors (Nothing : rest) = processErrors rest
  processErrors (Just e  : _)    = throwIO e

setReceiveHandler :: NIC -> (ByteString -> IO ()) -> IO ()
setReceiveHandler nic handler = do
  _ <- takeMVar (nicRxHandler nic)
  putMVar (nicRxHandler nic) handler

-- ----------------------------------------------------------------------------

nicTxRingType :: FrontEndRingType TxRequest TxResponse
nicTxRingType  = FrontEndRingType {
    entrySize    = max (#size netif_tx_request_t) (#size netif_tx_response_t)
  , peekResponse = readTxResponse
  , pokeRequest  = writeTxRequest
  }

data TxResponse = TxResponse {
    txrsId     :: Word16
  , txrsStatus :: Maybe ErrorCode
  }
 deriving (Show)

readTxResponse :: Ptr TxResponse -> IO TxResponse
readTxResponse ptr = do
  i  <- (#peek netif_tx_response_t, id)     ptr
  st <- (#peek netif_tx_response_t, status) ptr
  case st :: Int16 of
    (#const NETIF_RSP_DROPPED) -> return $ TxResponse i Nothing
    (#const NETIF_RSP_ERROR)   -> return $ TxResponse i (Just EIO)
    (#const NETIF_RSP_OKAY)    -> return $ TxResponse i Nothing
    _                          -> return $ TxResponse i (Just EPROTO)

data TxRequest = TxRequest {
    txrqGrant  :: GrantRef
  , txrqOffset :: Word16
  , txrqFlags  :: Word16
  , txrqId     :: Word16
  , txrqSize   :: Word16
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

nicRxRingType :: FrontEndRingType RxRequest RxResponse
nicRxRingType  = FrontEndRingType {
    entrySize     = max (#size netif_rx_request_t) (#size netif_rx_response_t)
  , peekResponse  = readRxResponse
  , pokeRequest   = writeRxRequest
  }

data RxResponse = RxResponse {
    rxrsId     :: Word16
  , rxrsOffset :: Word16
  , rxrsFlags  :: Word16
  , rxrsStatus :: Int16
  }
 deriving (Show)

readRxResponse :: Ptr RxResponse -> IO RxResponse
readRxResponse ptr = do
  i <- (#peek netif_rx_response_t,id)     ptr
  o <- (#peek netif_rx_response_t,offset) ptr
  f <- (#peek netif_rx_response_t,flags)  ptr
  s <- (#peek netif_rx_response_t,status) ptr
  return (RxResponse i o f s)

data RxRequest = RxRequest {
    rxrqId    :: Word16
  , rxrqGrant :: GrantRef
  }

writeRxRequest :: Ptr RxRequest -> RxRequest -> IO ()
writeRxRequest ptr req = do
  (#poke netif_rx_request_t,id) ptr   (rxrqId req)
  (#poke netif_rx_request_t,gref) ptr (unGrantRef (rxrqGrant req))

foreign import ccall unsafe "memset"
  memset :: Ptr a -> Word8 -> Word -> IO ()
