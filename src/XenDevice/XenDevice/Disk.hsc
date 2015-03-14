-- BANNERSTART --
-- Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- blank line for haddock
-- |Direct access to low-level disk devices. You probably want to use a file
-- system instead.
--
#include <stdint.h>
#include <stdlib.h>
#include "xen/xen.h"
#include "xen/io/blkif.h"
#include "xen/io/xenbus.h"

module XenDevice.Disk (
         Disk
       , listDisks
       , openDisk
       , readDisk
       , writeDisk
       , flushDiskCaches
       , diskWriteBarrier
#ifdef BLKIF_OP_DISCARD
       , discardRegion
#endif
       , diskName
       , isDiskReadOnly
       , isDiskRemovable
       , diskSupportsBarrier
       , diskSupportsFlush
       , diskSupportsDiscard
       , diskSectors
       , diskSectorSize
       )
 where

import Communication.RingBuffer
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as SBS
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Unsafe(unsafePackCStringFinalizer, unsafeUseAsCStringLen)
import Data.Int
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Memory
import Hypervisor.Port
import Hypervisor.XenStore

import Hypervisor.Debug

data Disk = Disk {
    diskName            :: String
  , diskHandle          :: Word16
  , diskRing            :: FrontEndRing DiskRequest DiskResponse
  , isDiskReadOnly      :: Bool
  , isDiskRemovable     :: Bool
  , diskSectors         :: Word
  , diskSectorSize      :: Word
  , diskSupportsBarrier :: Bool
  , diskSupportsFlush   :: Bool
  , diskSupportsDiscard :: Bool
  -- You may not hold both of these locks at the same time.
  , nextRequestIdMV     :: MVar Word64
  , requestTableMV      :: MVar (Map Word64 (MVar (Maybe ErrorCode)))
  }

advanceReqId :: Word64 -> Word64
advanceReqId x = if (x' == 0) then 1 else x'
 where x' = x + 1

-- |List all the disks available to the domain.
listDisks :: XenStore -> IO [String]
listDisks xs = do
  ents <- xsDirectory xs "device/vbd"
  forM (filter (not . null) ents) $ \ ent -> do
    backend <- xsRead xs ("device/vbd/" ++ ent ++ "/backend")
    xsRead xs (backend ++ "/dev")

-- |Attach to a disk device and prepare it for operation.
openDisk :: XenStore -> String -> IO Disk
openDisk xs name = do
  (feDir, beDir) <- findDevice xs name
  secSize <- read     `fmap` xsRead xs (beDir ++ "/sector-size")
  numSecs <- read     `fmap` xsRead xs (beDir ++ "/sectors")
  readOnl <- readMode `fmap` xsRead xs (beDir ++ "/mode")
  isRemov <- readRemv `fmap` xsRead xs (beDir ++ "/removable")
  dom     <- read     `fmap` xsRead xs (feDir ++ "/backend-id")
  hndle   <- read     `fmap` xsRead xs (feDir ++ "/virtual-device")
  --
  ring  <- frbCreate diskRingType (toDomId (dom :: Word))
  req   <- newMVar 1
  table <- newMVar Map.empty
  xsWrite xs (feDir ++ "/ring-ref") (show (unGrantRef (frbGetRef ring)))
  xsWrite xs (feDir++"/event-channel") (show (fromPort (frbGetPort ring)::Word))
  xsWrite xs (feDir ++ "/state") (show ((#const XenbusStateInitialised)::Word))
  waitForState (beDir ++ "/state") (show ((#const XenbusStateConnected)::Word))
  xsWrite xs (feDir ++ "/state") (show ((#const XenbusStateConnected)::Word))
  --
  canBarrier <- testFeature (beDir ++ "/feature-barrier")
  canFlush   <- testFeature (beDir ++ "/feature-flush-cache")
  canDiscard <- testFeature (beDir ++ "/feature-discard")
  --
  _ <- forkIO $ forever $ processResponses ring table
  return Disk {
    diskName            = name
  , diskHandle          = hndle
  , diskRing            = ring
  , isDiskReadOnly      = readOnl
  , isDiskRemovable     = isRemov
  , diskSectors         = numSecs
  , diskSectorSize      = secSize
  , diskSupportsBarrier = canBarrier
  , diskSupportsFlush   = canFlush
  , diskSupportsDiscard = canDiscard
  , nextRequestIdMV     = req
  , requestTableMV      = table
  }
 where
  readMode "r" = True
  readMode "w" = False
  readMode  _  = throw EREMOTEIO
  readRemv "0" = False
  readRemv "1" = True
  readRemv  _  = throw EIO
  --
  waitForState key val = do
    val' <- xsRead xs key
    unless (val' == val) $ do
      threadDelay 100000
      waitForState key val
  --
  testFeature key = handle (\ (_ :: ErrorCode) -> return False) $ do
    val <- xsRead xs key
    return (val == "1")
  --
  processResponses ring tableMV = do
    resps <- frbReadResponses ring
    modifyMVar_ tableMV $ \ table -> do
      foldM handleResponse table resps
  --
  handleResponse table resp =
    case Map.lookup (respId resp) table of
      Nothing -> do
        writeDebugConsole "WARNING: Received response to unsent disk request.\n"
        return table
      Just retMV -> do
        let retval = case respStatus resp of
                       (#const BLKIF_RSP_EOPNOTSUPP) -> Just EOPNOTSUPP
                       (#const BLKIF_RSP_ERROR)      -> Just EIO
                       (#const BLKIF_RSP_OKAY)       -> Nothing
                       _                             -> Just EPROTO
        putMVar retMV retval
        return (Map.delete (respId resp) table)


-- Given the name of a device, find its ID, if it's around.
findDevice :: XenStore -> String -> IO (String, String)
findDevice xs str = do
  ents <- xsDirectory xs "device/vbd"
  maps <- forM (filter (not . null) ents) $ \ ent -> do
            backend <- xsRead xs ("device/vbd/" ++ ent ++ "/backend")
            name <- xsRead xs (backend ++ "/dev")
            return (name, ("device/vbd/" ++ ent, backend))
  case lookup str maps of
    Nothing  -> throwIO ENODEV
    Just res -> return res

-- |Read data n bytes from the given sector on the disk. If n is not an even
-- multiple of the sector size, it will be rounded up to the nearest sector.
readDisk :: Disk -> Word -> Word -> IO ByteString
readDisk disk inlen sector = do
  oreqid <- takeMVar (nextRequestIdMV disk)
  (reqs, nextReq, bstr) <- buildReqs length' oreqid sector
  putMVar (nextRequestIdMV disk) nextReq
  mvs <- modifyMVar (requestTableMV disk) $ \ reqTable ->
           foldM addReqTableEntry (reqTable, []) reqs
  frbWriteRequests (diskRing disk) reqs
  errs <- mapM takeMVar mvs
  mapM_ endAccess (map bsGrant (concatMap segments reqs))
  case catMaybes errs of
    []    -> return bstr
    (x:_) -> throwIO x
 where
  numSecs     = (inlen + (diskSectorSize disk - 1)) `div` (diskSectorSize disk)
  length'     = numSecs * diskSectorSize disk
  secsPerPage = 4096 `div` diskSectorSize disk
  --
  buildReqs 0 reqid _ = return ([], reqid, BS.empty)
  buildReqs left myreq stsec = do
    (segs, fbstr, left', stsec') <- buildSegs left stsec
    (rreqs, rid, rbstr) <- buildReqs left' (advanceReqId myreq) stsec'
    let req = DiskRequest {
                reqOp       = BlockOpRead
              , numSegments = fromIntegral (length segs)
              , devHandle   = diskHandle disk
              , reqId       = myreq
              , sectorNum   = fromIntegral stsec
              , segments    = segs
              , numSectors  = undefined
              , secure      = undefined
              }
    return (req : rreqs, rid, fbstr `BS.append` rbstr)
  --
  buildSegs 0    stsec = return ([], BS.empty, 0, stsec)
  buildSegs left stsec = do
    let sectorsLeft = left `div` diskSectorSize disk
        mySectors   = min secsPerPage sectorsLeft
        myLength    = diskSectorSize disk * mySectors
        left'       = left - myLength
        stsec'      = stsec + mySectors
    page <- allocPage
    [ref] <- grantAccess (frbGetDomain (diskRing disk)) page 4096 True
    mybstr <- unsafePackCStringFinalizer page (fromIntegral myLength)
                (freePage page)
    let seg = Segment ref 0 (fromIntegral mySectors - 1)
    (rsegs, rbstr, 0, ensec) <- buildSegs left' stsec'
    return (seg : rsegs, BS.fromStrict mybstr `BS.append` rbstr, 0, ensec)

addReqTableEntry :: (Map Word64 (MVar (Maybe ErrorCode)),
                     [MVar (Maybe ErrorCode)]) ->
                    DiskRequest ->
                    IO (Map Word64 (MVar (Maybe ErrorCode)),
                        [MVar (Maybe ErrorCode)])
addReqTableEntry (inTable, rmvs) req = do
  mv <- newEmptyMVar
  return (Map.insert (reqId req) mv inTable, mv : rmvs)

-- |Write the bytestring to the given sector on the disk. The ByteString
-- provided should be an even multiple of sector size. The results if it
-- is not are not defined.
writeDisk :: Disk -> ByteString -> Word -> IO ()
writeDisk disk bs sector = do
  writeDebugConsole ("Writing bs starting with " ++ show (BS.take 16 bs) ++ "\n")
  (segs, pgs) <- segmentize Nothing (BS.toChunks bs)
  firstReqId <- takeMVar (nextRequestIdMV disk)
  (reqs, nextReqId) <- requestify segs sector firstReqId
  putMVar (nextRequestIdMV disk) $! nextReqId
  mvars <- modifyMVar (requestTableMV disk) $ \ reqTable ->
             foldM addReqTableEntry (reqTable, []) reqs
  frbWriteRequests (diskRing disk) reqs
  errs <- mapM takeMVar mvars
  mapM_ endAccess (map bsGrant segs)
  mapM_ freePage pgs
  case catMaybes errs of
    []      -> return ()
    (err:_) -> throwIO err
 where
  dom         = frbGetDomain (diskRing disk)
  --
  requestify :: [BlockSegment] -> Word -> Word64 -> IO ([DiskRequest], Word64)
  requestify []   _ nextReq =
    return ([], nextReq)
  requestify segs startSec myReq = do
    let (req1segs, osegs) = splitAt 11 segs
        nsecs             = foldr (\ a acc ->
                                    fromIntegral (bsLast a) -
                                    fromIntegral (bsFirst a) +
                                    1 + acc)
                                  0 req1segs
        request           = DiskRequest {
          reqOp       = BlockOpWrite
        , numSegments = fromIntegral (length req1segs)
        , devHandle   = diskHandle disk
        , reqId       = myReq
        , sectorNum   = fromIntegral startSec
        , segments    = req1segs
        , numSectors  = undefined
        , secure      = undefined
        }
    (rres, resreqid) <- requestify osegs (startSec + nsecs) (advanceReqId myReq)
    return (request : rres, resreqid)
  --
  segmentize Nothing [] = return ([], [])
  segmentize (Just (page, len)) [] = do
    [ref] <- grantAccess dom page 4096 False
    let endsec = fromIntegral ((len - 1)`div` 512)
    return ([Segment ref 0 endsec], [page])
  segmentize Nothing chunks = do
    pg <- allocPage
    segmentize (Just (pg, 0)) chunks
  segmentize info@(Just (page, len)) chunks@(chunk:rest)
    | SBS.length chunk == 0 = segmentize info rest
    | len == 4096           = do
       [ref] <- grantAccess dom page 4096 False
       (rsegs, rpages) <- segmentize Nothing chunks
       return (Segment ref 0 7 : rsegs, page : rpages)
    | otherwise             = do
       let (mypart, rchunk) = SBS.splitAt (4096 - len) chunk
       len' <- unsafeUseAsCStringLen mypart $ \ (ptr, plen) -> do
                 memcpy (page `plusPtr` len) ptr (fromIntegral plen)
                 return (len + plen)
       segmentize (Just (page, len')) (rchunk : rest)

-- |Create a write barrier on the disk; all writes prior to this request
-- will be completed before any commands after this event.
diskWriteBarrier :: Disk -> IO ()
diskWriteBarrier disk = do
  myid <- modifyMVar (nextRequestIdMV disk) (\ x -> return (advanceReqId x, x))
  let req = DiskRequest {
              reqOp       = BlockOpWriteBarrier
            , numSegments = 0
            , devHandle   = diskHandle disk
            , reqId       = myid
            , sectorNum   = 0
            , segments    = []
            , numSectors  = undefined
            , secure      = undefined
            }
  [mv] <- modifyMVar (requestTableMV disk) (\ x -> addReqTableEntry (x,[]) req)
  frbWriteRequests (diskRing disk) [req]
  err <- takeMVar mv
  case err of
    Nothing -> return ()
    Just e  -> throwIO e

-- |Flush any cached items out to disk.
flushDiskCaches :: Disk -> IO ()
flushDiskCaches disk = do
  myid <- modifyMVar (nextRequestIdMV disk) (\ x -> return (advanceReqId x, x))
  let req = DiskRequest {
              reqOp       = BlockOpFlushDiskCache
            , numSegments = 0
            , devHandle   = diskHandle disk
            , reqId       = myid
            , sectorNum   = 0
            , segments    = []
            , numSectors  = undefined
            , secure      = undefined
            }
  [mv] <- modifyMVar (requestTableMV disk) (\ x -> addReqTableEntry (x,[]) req)
  frbWriteRequests (diskRing disk) [req]
  err <- takeMVar mv
  case err of
    Nothing -> return ()
    Just e  -> throwIO e

#ifdef BLKIF_OP_DISCARD
-- |Indicate to the backend device that a region of storage is no longer in use
-- and may be discarded at any time without impact. If the boolean flag is
-- present, it also ensures that the discarded region is rendered
-- unrecoverable before the command returns.
--
-- You may also know this command as trim or unmap. The Word arguments are
-- the first sector and number of sectors to discard.
discardRegion :: Disk -> Word64 -> Word64 -> Bool -> IO ()
discardRegion disk sector num dosec = do
  myid <- modifyMVar (nextRequestIdMV disk) (\ x -> return (advanceReqId x, x))
  let req = DiskRequest {
              reqOp       = BlockOpDiscard
            , numSegments = 0
            , devHandle   = diskHandle disk
            , reqId       = myid
            , sectorNum   = sector
            , segments    = []
            , numSectors  = num
            , secure      = dosec
            }
  [mv] <- modifyMVar (requestTableMV disk) (\ x -> addReqTableEntry (x,[]) req)
  frbWriteRequests (diskRing disk) [req]
  err <- takeMVar mv
  case err of
    Nothing -> return ()
    Just e  -> throwIO e
#endif

-- ----------------------------------------------------------------------------

diskRingType :: FrontEndRingType DiskRequest DiskResponse
diskRingType = FrontEndRingType {
    entrySize    = max (#size blkif_request_t) (#size blkif_response_t)
  , peekResponse = readDiskResponse
  , pokeRequest  = writeDiskRequest
  }

data BlockOperation = BlockOpRead
                    | BlockOpWrite
                    | BlockOpWriteBarrier
                    | BlockOpFlushDiskCache
#ifdef BLKIF_OP_DISCARD
                    | BlockOpDiscard
#endif
 deriving (Eq)

instance Storable BlockOperation where
  sizeOf _    = 1
  alignment _ = 1
  peek ptr    = do
    val <- peek (castPtr ptr) :: IO Word8
    case val of
      (#const BLKIF_OP_READ)            -> return BlockOpRead
      (#const BLKIF_OP_WRITE)           -> return BlockOpWrite
      (#const BLKIF_OP_WRITE_BARRIER)   -> return BlockOpWriteBarrier
      (#const BLKIF_OP_FLUSH_DISKCACHE) -> return BlockOpFlushDiskCache
#ifdef BLKIF_OP_DISCARD
      (#const BLKIF_OP_DISCARD)         -> return BlockOpDiscard
#endif
      _                                 -> throwIO EIO
  poke ptr BlockOpRead =
    poke (castPtr ptr) ((#const BLKIF_OP_READ) :: Word8)
  poke ptr BlockOpWrite =
    poke (castPtr ptr) ((#const BLKIF_OP_WRITE) :: Word8)
  poke ptr BlockOpWriteBarrier =
    poke (castPtr ptr) ((#const BLKIF_OP_WRITE_BARRIER) :: Word8)
  poke ptr BlockOpFlushDiskCache =
    poke (castPtr ptr) ((#const BLKIF_OP_FLUSH_DISKCACHE) :: Word8)
#ifdef BLKIF_OP_DISCARD
  poke ptr BlockOpDiscard =
    poke (castPtr ptr) ((#const BLKIF_OP_DISCARD) :: Word8)
#endif

data DiskRequest = DiskRequest {
    reqOp       :: BlockOperation
  , numSegments :: Word8
  , devHandle   :: Word16
  , reqId       :: Word64
  , sectorNum   :: Word64
  , segments    :: [BlockSegment]
    -- these are only used for discard requests
  , numSectors  :: Word64
  , secure      :: Bool
  }

data BlockSegment = Segment {
    bsGrant     :: GrantRef
  , bsFirst     :: Word8
  , bsLast      :: Word8
  }

instance Storable BlockSegment where
  sizeOf _    = 8
  alignment _ = 1
  peek ptr    = do
    g <- GrantRef `fmap` (#peek struct blkif_request_segment,gref)       ptr
    f <-                 (#peek struct blkif_request_segment,first_sect) ptr
    l <-                 (#peek struct blkif_request_segment,last_sect)  ptr
    return (Segment g f l)
  poke ptr s  = do
    (#poke struct blkif_request_segment,gref)       ptr (unGrantRef (bsGrant s))
    (#poke struct blkif_request_segment,first_sect) ptr (bsFirst s)
    (#poke struct blkif_request_segment,last_sect)  ptr (bsLast s)

data DiskResponse = DiskResponse {
    respId      :: Word64
  , _respOp     :: BlockOperation
  , respStatus  :: Int16
  }

writeDiskRequest :: Ptr DiskRequest -> DiskRequest -> IO ()
writeDiskRequest ptr req = do
#ifdef BLKIF_OP_DISCARD
  (#poke blkif_request_t,operation) ptr (reqOp req)
  if reqOp req == BlockOpDiscard
    -- write a blkif_reqeust_discard
    then do (#poke blkif_request_discard_t,flag)          ptr discardFlag
            (#poke blkif_request_discard_t,handle)        ptr (devHandle req)
            (#poke blkif_request_discard_t,id)            ptr (reqId req)
            (#poke blkif_request_discard_t,sector_number) ptr (sectorNum req)
            (#poke blkif_request_discard_t,nr_sectors)    ptr (numSectors req)
    -- write a blkif_request
    else do
#endif
            (#poke blkif_request_t,nr_segments)   ptr numSegs
            (#poke blkif_request_t,handle)        ptr (devHandle req)
            (#poke blkif_request_t,id)            ptr (reqId req)
            (#poke blkif_request_t,sector_number) ptr (sectorNum req)
            pokeArray (castPtr ptr `plusPtr` (#offset blkif_request_t,seg))
                      (segments req)
 where
  numSegs     = fromIntegral (length (segments req)) :: Word8
#ifdef BLKIF_OP_DISCARD
  discardFlag = if (secure req) then (#const BLKIF_DISCARD_SECURE) else 0::Word8
#endif

readDiskResponse :: Ptr DiskResponse -> IO DiskResponse
readDiskResponse ptr = do
  ident  <- (#peek blkif_response_t,id)        ptr
  op     <- (#peek blkif_response_t,operation) ptr
  status <- (#peek blkif_response_t,status)    ptr
  return (DiskResponse ident op status)


foreign import ccall unsafe "memcpy"
  memcpy :: Ptr a -> Ptr a -> Word -> IO ()
