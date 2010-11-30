-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- blank line for haddock
-- |Direct access to low-level disk devices. You probably want to use a file
-- system instead.
--
module XenDevice.Disk (dDisks,
                       Disk,
                       initializedDisks, potentialDisks,
                       notifyOnNewInitializedDisk, notifyOnNewPotentialDisk,
                       diskIsInitialized, initializeDisk,
                       diskName,
                       diskIsReadOnly, diskIsRemovable, diskIsCDRom,
                       diskSectors, diskBytesPerSector,
                       DResult(..),
                       readBytes, tryReadBytes,
                       writeBytes, tryWriteBytes)
    where

import XenDevice.Xenbus as XB
import XenDevice.XenRingState

import Communication.RingBuffer
import Control.Concurrent.MVar
import Control.Exception(assert)
import Control.Monad
import Data.Bits
import Data.Int
import Data.IORef
import Data.List
import Data.Word
import GHC.IOBase(unsafePerformIO)
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Basics
import Hypervisor.Debug
import Hypervisor.Kernel
import Hypervisor.Memory

-- The implementation of the public interfaces

-- LOCK ORDERING:
--
--   initializedDeviceMV
--   potentialDeviceMV
--   initializationCallbacksMV
--   potentialCallbacksMV
--
-- Thus, if you need to take initializedDeviceMV and
-- initializationCallbacksMV, you must lock them in that
-- order. Taking out locks in another order may cause a
-- deadlock.

dDisks :: DeviceDriver
dDisks = DeviceDriver { devName = "XenDeviceInternalDisks"
                      , dependencies = [XB.dXenbus]
                      , initialize = initDiskDriver
                      , shutdown = return ()
                      }

-- |Computes and returns the list of initialized disks.
initializedDisks :: IO [Disk]
initializedDisks = readMVar initializedDisksMV

-- |Computes and returns the list of potential disks. These are disks
-- that Xen has made available, but have not had initializeDisk called
-- on them.
potentialDisks :: IO [String]
potentialDisks =
    do devices <- readMVar potentialDisksMV
       return $ map pDiskName devices

-- |Installs a handler that will be called when a disk is initialized.
-- Note that this handler will be called regardless of the originating
-- thread; if a thread installs a callback and then initializes the 
-- disk, it will still receive the event.
notifyOnNewInitializedDisk :: (Disk -> IO ()) -> IO ()
notifyOnNewInitializedDisk callback =
    do lock <- takeMVar initializedDisksMV
       iCallbacks <- takeMVar initializationCallbacksMV
       putMVar initializationCallbacksMV (callback:iCallbacks)
       putMVar initializedDisksMV lock

-- |Installs a handler that will be called when Xen informs the domain
-- of a new, potential disk. 
notifyOnNewPotentialDisk :: (String -> IO ()) -> IO ()
notifyOnNewPotentialDisk callback =
    do lock <- takeMVar potentialDisksMV
       pCallbacks <- takeMVar potentialCallbacksMV
       putMVar potentialCallbacksMV (callback:pCallbacks)
       putMVar potentialDisksMV lock

-- |Determines whether or not the given disk is initialized
diskIsInitialized :: String -> IO Bool
diskIsInitialized disk =
    do devices <- readMVar initializedDisksMV
       return $ elem disk $ map iDiskName devices

-- |Intialize a disk, so that it can be used to read and write, and the
-- various bits of information about it (whether or not it is read only,
-- the number of sectors, etc.) becomes available. Returns the initialized
-- disk upon success. This routine can be called safely on the same disk
-- multiple times.
initializeDisk :: String -> IO (Maybe Disk)
initializeDisk dName = do
  iDevices <- readMVar initializedDisksMV
  case find (\ dev -> iDiskName dev == dName) iDevices of
    Just disk -> return (Just disk)
    Nothing   -> do 
      devices <- readMVar potentialDisksMV
      case find (\ dev -> pDiskName dev == dName) devices of
        Nothing  -> return Nothing
        Just dev -> do
          asyncNotifMV <- newEmptyMVar
          -- We must put the lock back immediately so that we don't
          -- accidentally block anyone else
          probeDisk dev asyncNotifMV
          retval <- takeMVar asyncNotifMV
          case retval of
            Nothing   -> return ()
            Just disk -> do
              iDevs <- takeMVar initializedDisksMV
              pDevs <- takeMVar potentialDisksMV
              putMVar potentialDisksMV
                (filter (\ x -> pDiskName x /= dName) pDevs)
              putMVar initializedDisksMV (disk:iDevs)
          return retval

-- |Returns the name associated with the given disk
diskName :: Disk -> String
diskName = iDiskName

-- |Determines whether or not the given disk is read-only
diskIsReadOnly :: Disk -> Bool
diskIsReadOnly = iDiskIsReadOnly

-- |Determines whether or not the given disk is removable
diskIsRemovable :: Disk -> Bool
diskIsRemovable = iDiskIsRemovable

-- |Determines whether or not the given disk is a CDRom
diskIsCDRom :: Disk -> Bool
diskIsCDRom = iDiskIsCDRom

-- |Determines how many sectors there are on the given disk.
diskSectors :: Disk -> Word64
diskSectors = iDiskNumSectors

-- |Determines how many bytes there are per sector on the given disk.
diskBytesPerSector :: Disk -> Word32
diskBytesPerSector = iDiskBytesPerSector

-- |From the given disk, read size (arg3) bytes starting at the
-- given sector (arg2), storing the data in the given pages (arg3). 
-- The data is stored in-order by the list; a 16K read using <1,2,3,4>
-- as the pages will store the first 4K in page 1, the second 4K in
-- page 2, and so on. This routing blocks execution until the operation
-- completes. 
readBytes :: Disk -> Word64 -> Word32 -> [VPtr a] -> IO DResult
readBytes disk sector size pages = 
    argumentsCheckOut disk DiskRead sector size pages (\ reqs -> do
      reses <- frbRequestMany (iDiskRing disk) reqs
      return $ zipResults reses)

-- |The same as readBytes, but immediately returns if the underlying disk
-- driver buffer is full. 
tryReadBytes :: Disk -> Word64 -> Word32 -> [VPtr a] -> IO DResult
tryReadBytes disk sector size pages = 
    argumentsCheckOut disk DiskRead sector size pages (\ reqs -> do
      _reses <- frbTryRequestMany (iDiskRing disk) reqs
      case _reses of
        Just reses -> return $ zipResults reses
        Nothing -> return DRErrDeviceBusy)

-- |Write size (arg3) bytes to the given disk at the given sector (arg2),
-- using the data in the given pages. This routine will block until the
-- operation succeeds or fails.
writeBytes :: Disk -> Word64 -> Word32 -> [VPtr a] -> IO DResult
writeBytes disk sector size pages =
    argumentsCheckOut disk DiskWrite sector size pages (\ reqs -> do
      reses <- frbRequestMany (iDiskRing disk) reqs
      return $ zipResults reses)

-- |The same as writeBytes, but immediately returns if the underlying disk
-- driver's buffer is full.
tryWriteBytes :: Disk -> Word64 -> Word32 -> [VPtr a] -> IO DResult
tryWriteBytes disk sector size pages = 
    argumentsCheckOut disk DiskWrite sector size pages (\ reqs -> do
      _reses <- frbTryRequestMany (iDiskRing disk) reqs
      case _reses of
        Just reses -> return $ zipResults reses
        Nothing -> return DRErrDeviceBusy)

-- ---------------------------------------------------------------------------
--
-- INTERNAL
-- Maintenance (connection and shutdown) of a device
--
-- ---------------------------------------------------------------------------

data PotentialDisk = PotentialDisk { 
    pDiskName :: String
  , pDiskNodeName :: String
  , pDiskHandle :: Word16
  , pDiskBackend :: String
  , pDiskBackendDomId :: DomId
  }

data Disk = Disk { iDiskName :: String
  , iDiskHandle :: Word16
  , iDiskBackendDomId :: DomId
  , iDiskRing :: DiskRingBuffer
  , iDiskIsReadOnly :: Bool
  , iDiskIsRemovable :: Bool
  , iDiskIsCDRom :: Bool
  , iDiskNumSectors :: Word64
  , iDiskBytesPerSector :: Word32
  }

{-# NOINLINE initializedDisksMV #-}
-- This is the global state required for this driver. 
initializedDisksMV :: MVar [Disk]
initializedDisksMV = unsafePerformIO $ newMVar []

{-# NOINLINE potentialDisksMV #-}
potentialDisksMV :: MVar [PotentialDisk]
potentialDisksMV = unsafePerformIO $ newMVar []

{-# NOINLINE initializationCallbacksMV #-}
initializationCallbacksMV :: MVar [Disk -> IO ()]
initializationCallbacksMV = unsafePerformIO $ newMVar []

{-# NOINLINE potentialCallbacksMV #-}
potentialCallbacksMV :: MVar [String -> IO ()]
potentialCallbacksMV = unsafePerformIO $ newMVar []

-- Just a handy little thing
runCallbacks :: MVar [a -> IO ()] -> a -> IO ()
runCallbacks callbacksMV val =
    do callbacks <- readMVar callbacksMV
       mapM_ (\ cb -> cb val) callbacks

-- This initializes the device driver, by pulling the available information
-- from the Xenstore about the initial potential devices and then storing
-- that away in our list of potential devices.
initDiskDriver :: IO ()
initDiskDriver = 
    do ret <- XB.xsDirectory "device/vbd"
       case ret of
         XBOk vbd_devs -> mapM_ generatePotentialDevice vbd_devs
         _ -> hDEBUG "VBD: No disk devices found on initial inspection.\n"
       ret' <- XB.xsSetWatch "device/vbd" onVBDModification
       case ret' of
         XBOk _ -> hDEBUG "VBD: Set new device watch on device/vbd.\n"
         XBError msg -> hDEBUG $ "VBD: Couldn't set watch on device/vbd! (" ++ 
                                 msg ++ ")\n"

-- Invoked by the Xenbus when Xen adds a new device to our local tree.
onVBDModification :: WatchId -> String -> IO ()
onVBDModification _ path =
    unless (any (\x -> x == '/') (drop (length "device/vbd") path))
           (generatePotentialDevice (drop (length "device/vbd") path))

-- Called from the initialization, this attempts to generate a PotentialDisk
-- from an entry in our initial XenStore
generatePotentialDevice :: String -> IO ()
generatePotentialDevice _nodeName | _nodeName == "" = return ()
generatePotentialDevice _nodeName =
    do let nodeName = "device/vbd/" ++ _nodeName
       _vdStr       <- XB.xsRead $ nodeName ++ "/virtual-device"
       _bidStr      <- XB.xsRead $ nodeName ++ "/backend-id"
       _stateStr    <- XB.xsRead $ nodeName ++ "/state"
       _backend     <- XB.xsRead $ nodeName ++ "/backend"
       case (_vdStr, _bidStr, _stateStr, _backend) of
         (XBOk _, XBOk bidStr, XBOk _, XBOk backend) ->
             do _name        <- XB.xsRead $ backend ++ "/dev"
                case _name of
                  XBOk name ->
                      do let (virt_dev_id::Word16) = read _nodeName
                             (backend_id::Word16) = read bidStr
                         hDEBUG $ "VBD: Found potential block device " ++ 
                                  name ++ "\n"
                         pDevices <- takeMVar potentialDisksMV 
                         let newDev = PotentialDisk { pDiskName = name
                                                    , pDiskHandle = virt_dev_id
                                                    , pDiskBackend = backend
                                                    , pDiskBackendDomId = 
                                                        (DomId backend_id)
                                                    , pDiskNodeName = nodeName
                                                    }
                         runCallbacks potentialCallbacksMV name
                         putMVar potentialDisksMV (newDev:pDevices)
                  _ -> maybePrintCaseError _name "disk name"
         _ -> do maybePrintCaseError _vdStr "virtual device"
                 maybePrintCaseError _bidStr "backend id"
                 maybePrintCaseError _stateStr "state"
                 maybePrintCaseError _backend "backend path"
    where maybePrintCaseError :: XBResult a -> String -> IO () 
          maybePrintCaseError (XBOk _) _ = return ()
          maybePrintCaseError (XBError _) item =
              hDEBUG $ "VBD: Couldn't get " ++ item ++ " for device node " ++ 
                       _nodeName ++ "\n"

-- Called to attempt to initialize a potential device. 
probeDisk :: PotentialDisk -> MVar (Maybe Disk) -> IO ()
probeDisk dev resMV = do
  _frb <- xTry $ frbCreate (pDiskBackendDomId dev)
  case _frb of
    Right (frb, (GrantRef grefNum), port) -> do
      ret <- XB.xsSetWatch ((pDiskBackend dev) ++ "/state") 
                           (onPossibleConnect dev frb resMV)
      case ret of
        XBOk _ -> do
          XB.xsWrite ((pDiskNodeName dev)++"/ring-ref") (show grefNum)
          XB.xsWrite ((pDiskNodeName dev)++"/event-channel") 
                     (drop 1 $ dropWhile (/= ' ') $ show port)
          XB.xsWrite ((pDiskNodeName dev)++"/state") (show xenRingInitialised)
          return ()
        XBError _ -> do
          hDEBUG $ "VBD: Failed to set conn watch for disk "++
                   (pDiskName dev)++"\n"
          frbShutdown frb
          putMVar resMV Nothing
    Left e -> do
      hDEBUG $ "VBD: Failed to create ring buffer for disk " ++ 
               (pDiskName dev) ++ " (" ++ show e ++ ")\n"
      putMVar resMV Nothing
                              
onPossibleConnect :: PotentialDisk -> DiskRingBuffer -> MVar (Maybe Disk) ->
                     WatchId -> String -> IO ()
onPossibleConnect dev frb resMV watch path = do
  _stateStr <- XB.xsRead path
  case _stateStr of
    XBOk _state | (read _state) == xenRingInitialising -> return ()
    XBOk _state | (read _state) == xenRingInitialisingWait -> return ()
    XBOk _state | (read _state) == xenRingInitialised -> return ()
    XBOk _state | (read _state) == xenRingConnected -> do
      _dinfo <- gatherDiskInfo (pDiskBackend dev)
      case _dinfo of
        Just (sectors, sectorSize, info, _) -> do
          let (cdrom, removable, readonly) = parseInfo info
          _ <- XB.xsUnsetWatch watch
          XB.xsWrite ((pDiskNodeName dev) ++ "/state") (show xenRingConnected)
          hDEBUG $ "VBD: Connected to disk " ++ (pDiskName dev) ++ "\n"
          let newDisk = Disk { 
                          iDiskName = pDiskName dev
                        , iDiskHandle = pDiskHandle dev
                        , iDiskBackendDomId = pDiskBackendDomId dev
                        , iDiskRing = frb
                        , iDiskIsReadOnly = readonly
                        , iDiskIsRemovable = removable
                        , iDiskIsCDRom = cdrom
                        , iDiskNumSectors = sectors
                        , iDiskBytesPerSector = sectorSize
                        }
          runCallbacks initializationCallbacksMV newDisk
          putMVar resMV $ Just newDisk
        Nothing -> do
          hDEBUG $ "VBD: ERROR: No disk info for " ++ pDiskName dev ++
                   "?!\n"
          putMVar resMV Nothing
    XBOk _state | (read _state) == xenRingClosing -> fail'
    XBOk _state | (read _state) == xenRingClosed -> fail'
    XBOk _state -> do
      hDEBUG $ "VBD: Weird state " ++ show _state
      putMVar resMV Nothing
    XBError _ -> do
      hDEBUG $ "VBD: Error reading post-watch state for disk " ++ 
               (pDiskName dev) ++ "\n"
      putMVar resMV Nothing
 where fail' = do 
        hDEBUG $ "VBD: State failure (closure) for disk "++(pDiskName dev)++"\n"
        putMVar resMV Nothing

-- ---------------------------------------------------------------------------
--
-- INTERNAL
-- Validation and performance of I/O
--
-- ---------------------------------------------------------------------------

-- |The possible results from a disk operation.
data DResult = DROK 
               -- ^The operation was successful
             | DRErrDeviceBusy 
               -- ^You called one of the Try functions, and the internal 
               -- buffer was full.
             | DRErrInvalidSectorOrSize
               -- ^The sector and size information you sent would have run
               -- off the beginning or end of the disk.
             | DRErrMisalignedSize
               -- ^The size you sent was not a multiple of the sector size.
             | DRErrNotEnoughPages
               -- ^You didn't pass in enough pages to cover the size.
             | DRErrNoGrantsLeft
               -- ^There were an insufficient number of grant references left
               -- to perform the request
             | DRErrOperationNotSupported
               -- ^The operation you requested is not supported by the disk
             | DRErrDiskFailure
               -- ^The disk itself reported a failure in the operation
    deriving (Show,Ord,Eq)

-- Given a list of results, combine them into a single result that we can
-- return to the user. This functions by basically returning the first error
-- result.
zipResults :: [DiskResponse] -> DResult
zipResults [] = DROK
zipResults ((DiskResponse{ drspStatus = (#const BLKIF_RSP_EOPNOTSUPP) }):_) =
    DRErrOperationNotSupported
zipResults ((DiskResponse{ drspStatus = (#const BLKIF_RSP_ERROR) }):_) =
    DRErrDiskFailure
zipResults ((DiskResponse{ drspStatus = (#const BLKIF_RSP_OKAY) }):rest) =
    zipResults rest
zipResults _ =
    DRErrDiskFailure

-- Make sure the arguments passed to this disk operation are valid. If they
-- are, convert them into DiskRequest objects and pass them onto the passed-in
-- function to complete the operation. Otherwise, inform the user on why the
-- operation failed.
argumentsCheckOut :: Disk -> DiskOperation -> 
                     Word64 -> Word32 -> [VPtr a] ->
                     ([DiskRequest] -> IO DResult) ->
                     IO DResult
argumentsCheckOut disk op sector size pages k
  | size `mod` iDiskBytesPerSector disk /= 0 = return DRErrMisalignedSize
  | genericLength pages * 4096 < size        = return DRErrNotEnoughPages
  | calcSectors > iDiskNumSectors disk       = return DRErrInvalidSectorOrSize
  | otherwise                                = do
    (requests, status) <- spliceIntoRequests sector size pages
    fromDROK status (return status) $ do
      res <- k requests
      cleanUpRequests requests
      return res
 where
  calcSectors = sector + fromIntegral (size `div` iDiskBytesPerSector disk)

  requestSize = 11 * 4096

  fromDROK s def m | s == DROK = m
                   | otherwise = def

  spliceIntoRequests :: Word64 -> Word32 -> [VPtr a] ->
                        IO ([DiskRequest], DResult)
  spliceIntoRequests _ 0 _ = return ([], DROK)
  spliceIntoRequests sec s ps | s <= requestSize = do
    (segs, status) <- spliceIntoSegments s ps
    fromDROK status (return ([],status)) $ do
      reqId <- nextId
      let request = DiskRequest {
                      dreqOperation = op
                    , dreqHandle = iDiskHandle disk
                    , dreqId = reqId
                    , dreqSector = sec
                    , dreqSegments = segs
                    }
      return ([request], DROK)
  spliceIntoRequests sec s ps = do
    let sec' = sec + fromIntegral (requestSize `div` iDiskBytesPerSector disk)
    let s' = s - requestSize
    let (as,bs) = splitAt 11 ps
    (rest, status) <- spliceIntoRequests sec' s' bs
    fromDROK status (return ([],status)) $ do
      (first, status') <- spliceIntoRequests sec requestSize as
      let cleanup = cleanUpRequests rest >> return ([],status')
      fromDROK status' cleanup $ do
        () <- assert (length first == 1) $ return ()
        return (first ++ rest, status')
  -- --------------------------------------------------------------------
  spliceIntoSegments :: Word32 -> [VPtr a] -> 
                        IO ([DiskRequestSegment], DResult)
  spliceIntoSegments 0 _ = return ([], DROK)
  spliceIntoSegments s (fpage:_) | s <= 4096 = do
    _ref <- xTry allocRef 
    case _ref of
      Right ref -> do 
        grantAccess ref (iDiskBackendDomId disk) fpage True
        let sectors = (s `div` (iDiskBytesPerSector disk)) - 1
        let segment = DiskRequestSegment {
                        dsegGrant = ref
                      , dsegFirstSect = 0
                      , dsegLastSect = fromIntegral sectors
                      }
        return ([segment], DROK)
      Left _ -> do
        return ([], DRErrNoGrantsLeft)
  spliceIntoSegments s (fpage:rpages) = do
    (first, status) <- spliceIntoSegments 4096 [fpage]
    fromDROK status (return ([],status)) $ do
      () <- assert (length first == 1) $ return ()
      (rest, status') <- spliceIntoSegments (s - 4096) rpages
      let cleanup = cleanUpSegments first >> return ([],status')
      fromDROK status cleanup (return (first ++ rest, status'))
  spliceIntoSegments _ [] =
    error "INTERNAL ERROR: XenDevice.Disk.spliceIntoSegments, shouldn't be here"
  -- --------------------------------------------------------------------
  cleanUpRequests :: [DiskRequest] -> IO ()
  cleanUpRequests reqs = mapM_ (cleanUpSegments . dreqSegments) reqs
  -- --------------------------------------------------------------------
  cleanUpSegments :: [DiskRequestSegment] -> IO ()
  cleanUpSegments segs = mapM_ cleanupRef segs
    where
    cleanupRef DiskRequestSegment { dsegGrant = ref } = do
      endAccess ref
      freeRef ref

{-# NOINLINE currentId #-}
currentId :: IORef Word64
currentId = unsafePerformIO $ newIORef 0

nextId :: IO Word64
nextId = atomicModifyIORef currentId (\ x -> (x + 1, x))

-- ---------------------------------------------------------------------------
--
-- INTERNAL
-- Other internals not dealing with the ring buffer
--
-- ---------------------------------------------------------------------------

gatherDiskInfo :: String -> IO (Maybe (Word64, Word32, Word32, String))
gatherDiskInfo path = do
  resNumSecs <- XB.xsRead $ path ++ "/sectors"
  resSecSize <- XB.xsRead $ path ++ "/sector-size"
  resInfo <- XB.xsRead $ path ++ "/info"
  resName <- XB.xsRead $ path ++ "/dev"
  case (resNumSecs, resSecSize, resInfo, resName) of
    (XBOk numSecsStr, XBOk secSizeStr, XBOk infoStr, XBOk name) ->
        return $ Just $ ((read numSecsStr), (read secSizeStr), (read infoStr), name)
    _ -> return Nothing

parseInfo :: Word32 -> (Bool, Bool, Bool)
parseInfo info = ( (testBit info 0), (testBit info 1), (testBit info 2) )

hDEBUG :: String -> IO ()
hDEBUG = Hypervisor.Debug.writeDebugConsole

-- ---------------------------------------------------------------------------
--
-- INTERNAL
-- The implementation of the VBD ring buffer
--
-- ---------------------------------------------------------------------------

#include "xen/xen.h"
#include "xen/io/blkif.h"

type DiskRingBuffer = FrontRingBuffer DiskRequest DiskResponse Word64

data DiskRequest = DiskRequest {
    dreqOperation   :: DiskOperation
  , dreqHandle      :: Word16
  , dreqId          :: Word64
  , dreqSector      :: Word64
  , dreqSegments    :: [DiskRequestSegment]
  }

data DiskOperation = DiskRead
                   | DiskWrite

data DiskRequestSegment = DiskRequestSegment {
    dsegGrant     :: GrantRef
  , dsegFirstSect :: Word8
  , dsegLastSect  :: Word8
  }
  deriving (Show)
  
data DiskResponse = DiskResponse {
    drspId        :: Word64
  , drspOperation :: DiskOperation
  , drspStatus    :: Int16
  }

num2operation :: Word8 -> DiskOperation
num2operation (#const BLKIF_OP_READ) = DiskRead
num2operation (#const BLKIF_OP_WRITE) = DiskWrite
num2operation _ = error "Unknown value to convert to disk op!"

operation2num :: DiskOperation -> Word8
operation2num DiskRead = (#const BLKIF_OP_READ)
operation2num DiskWrite = (#const BLKIF_OP_WRITE)

instance RingBufferable DiskRequest DiskResponse Word64 where
  requestId DiskRequest{ dreqId = rid } = rid
  responseId DiskResponse{ drspId = rid } = rid
  entrySize _ _ = max (#size blkif_request_t) (#size blkif_response_t)

instance FrontRingBufferable DiskRequest DiskResponse Word64 where
  peekResponse ptr = do
    rid <- (#peek blkif_response_t, id) (castPtr ptr)
    op_num <- (#peek blkif_response_t, operation) (castPtr ptr)
    stat <- (#peek blkif_response_t, status) (castPtr ptr)
    return DiskResponse { 
             drspId = rid
           , drspOperation = num2operation op_num
           , drspStatus = stat 
           }
  pokeRequest ptr request = do
    (#poke blkif_request_t, operation) (castPtr ptr) 
           (operation2num (dreqOperation request))
    (#poke blkif_request_t, nr_segments) (castPtr ptr)
           (length (dreqSegments request))
    (#poke blkif_request_t, handle) (castPtr ptr) (dreqHandle request)
    (#poke blkif_request_t, id) (castPtr ptr) (dreqId request)
    (#poke blkif_request_t, sector_number) (castPtr ptr) (dreqSector request)
    zipWithM_ (\ seg ptr' -> do
                 let (GrantRef gref_num) = (dsegGrant seg)
                 (#poke struct blkif_request_segment, gref) (castPtr ptr')
                              gref_num
                 (#poke struct blkif_request_segment, first_sect)
                              (castPtr ptr')
                              (dsegFirstSect seg)
                 (#poke struct blkif_request_segment, last_sect) 
                              (castPtr ptr')
                              (dsegLastSect seg))
              (dreqSegments request)
              (iterate (\ ptr' -> 
                          ptr' `plusPtr` (#size struct blkif_request_segment))
                       (ptr `plusPtr` (#offset blkif_request_t, seg)))

