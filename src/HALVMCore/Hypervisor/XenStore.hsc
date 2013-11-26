-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.XenStore(
         XenStore
       , TransId
       , XSPerm(..)
       , initXenStore
       , initCustomXenStore
       , emptyTransaction
       , xsGetDomId
       , xsDirectory, xstDirectory
       , xsRead, xstRead
       , xsGetPermissions, xstGetPermissions
       , xsWatch
       , xsUnwatch, xstUnwatch
       , xsStartTransaction, xstStartTransaction
       , xstAbort, xstCommit
       , xsIntroduce, xstIntroduce
       , xsRelease, xstRelease
       , xsGetDomainPath, xstGetDomainPath
       , xsWrite, xstWrite
       , xsMakeDirectory, xstMakeDirectory
       , xsRemove, xstRemove
       , xsSetPermissions, xstSetPermissions
       , xsIsDomainIntroduced, xstIsDomainIntroduced
       , xsResume, xstResume
       , xsSetTarget, xstSetTarget
       , xsRestrict, xstRestrict
#ifdef XS_RESET_WATCHES
       , xsResetWatches, xstResetWatches
#endif
       )
 where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Hypervisor.Debug
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Memory
import Hypervisor.Port

data XenStore = XenStore (Chan DriverReq)

data XSPerm =
    WritePerm     DomId
  | ReadPerm      DomId
  | ReadWritePerm DomId
  | NonePerm      DomId
 deriving (Eq, Show, Generic)

newtype ReqId   = ReqId { unReqId :: Word32 }
 deriving (Eq, Ord, Show)

advanceReqId :: ReqId -> ReqId
advanceReqId (ReqId x) = ReqId (if x + 1 == 0 then 1 else x + 1)

newtype TransId = TransId { unTransId :: Word32 }
 deriving (Eq, Generic, Show)

emptyTransaction :: TransId
emptyTransaction  = TransId 0

initXenStore :: IO XenStore
initXenStore  = do
  xsMFN  <- (toMFN . fromIntegral) <$> get_xenstore_mfn
  xsPort <- toPort <$> get_xenstore_evtchn
  initCustomXenStore xsMFN xsPort

initCustomXenStore :: MFN -> Port -> IO XenStore
initCustomXenStore xsMFN xsPort = do
  commChan <- newChan
  xsPtr    <- handle (mapMFN xsMFN) (mfnToVPtr xsMFN)
  forkIO_ (xenbusClient xsPtr xsPort commChan initialClientState)
  setPortHandler xsPort $ writeChan commChan Advance
  return (XenStore commChan)
 where
  mapMFN :: MFN -> ErrorCode -> IO (VPtr a)
  mapMFN mfn _ = mapFrames [mfn]

xsGetDomId :: XenStore -> IO DomId
xsGetDomId xs = do
  val <- xsRead xs "domid"
  case reads val :: [(Word16, String)] of
    [(domid, "")] -> return (toDomId domid)
    _             -> throw EPROTO

xsDirectory :: XenStore -> String -> IO [String]
xsDirectory xs = xstDirectory xs emptyTransaction

xstDirectory :: XenStore -> TransId -> String -> IO [String]
xstDirectory (XenStore commChan) tid str =
  standardRequest commChan tid (XSDir str) RTDir parseStrings

xsRead :: XenStore -> String -> IO String
xsRead xs = xstRead xs emptyTransaction

xstRead :: XenStore -> TransId -> String -> IO String
xstRead (XenStore commChan) tid str =
  standardRequest commChan tid (XSRead str) RTRead parseString

xsGetPermissions :: XenStore -> String -> IO [XSPerm]
xsGetPermissions xs = xstGetPermissions xs emptyTransaction

xstGetPermissions :: XenStore -> TransId -> String -> IO [XSPerm]
xstGetPermissions (XenStore commChan) tid str =
  standardRequest commChan tid (XSGetPerms str) RTGetPerms parsePerms

xsWatch :: XenStore -> String -> String -> (String -> String -> IO ()) -> IO ()
xsWatch (XenStore commChan) str token handler = do
  respMV <- newEmptyMVar
  writeChan commChan (WatchRequest str token handler respMV)
  processResponse RTWatch (const ()) =<< takeMVar respMV

xsUnwatch :: XenStore -> String -> IO ()
xsUnwatch xs = xstUnwatch xs emptyTransaction

xstUnwatch :: XenStore -> TransId -> String -> IO ()
xstUnwatch (XenStore commChan) tid str =
  standardRequest commChan tid (XSUnwatch str) RTUnwatch (const ())

xsStartTransaction :: XenStore -> IO TransId
xsStartTransaction xs = xstStartTransaction xs emptyTransaction

xstStartTransaction :: XenStore -> TransId -> IO TransId
xstStartTransaction (XenStore commChan) tid =
  standardRequest commChan tid XSTransSt RTTransSt parseTransId

xstAbort :: XenStore -> TransId -> IO ()
xstAbort (XenStore commChan) tid =
  standardRequest commChan tid (XSTransEnd False) RTTransEnd (const ())

xstCommit :: XenStore -> TransId -> IO ()
xstCommit (XenStore commChan) tid =
  standardRequest commChan tid (XSTransEnd True) RTTransEnd (const ())

xsIntroduce :: XenStore -> DomId -> MFN -> Port -> IO ()
xsIntroduce xs = xstIntroduce xs emptyTransaction

xstIntroduce :: XenStore -> TransId -> DomId -> MFN -> Port -> IO ()
xstIntroduce (XenStore commChan) tid d m p =
  standardRequest commChan tid (XSIntro d m p) RTIntro (const ())

xsRelease :: XenStore -> DomId -> IO ()
xsRelease xs = xstRelease xs emptyTransaction

xstRelease :: XenStore -> TransId -> DomId -> IO ()
xstRelease (XenStore commChan) tid d =
  standardRequest commChan tid (XSRelease d) RTRelease (const ())

xsGetDomainPath :: XenStore -> DomId -> IO String
xsGetDomainPath xs = xstGetDomainPath xs emptyTransaction

xstGetDomainPath :: XenStore -> TransId -> DomId -> IO String
xstGetDomainPath (XenStore commChan) tid d =
  standardRequest commChan tid (XSGetPath d) RTGetPath parseString

xsWrite :: XenStore -> String -> String -> IO ()
xsWrite xs = xstWrite xs emptyTransaction

xstWrite :: XenStore -> TransId -> String -> String -> IO ()
xstWrite (XenStore commChan) tid k v =
  standardRequest commChan tid (XSWrite k v) RTWrite (const ())

xsMakeDirectory :: XenStore -> String -> IO ()
xsMakeDirectory xs = xstMakeDirectory xs emptyTransaction

xstMakeDirectory :: XenStore -> TransId -> String -> IO ()
xstMakeDirectory (XenStore commChan) tid d =
  standardRequest commChan tid (XSMkDir d) RTMkDir (const ())

xsRemove :: XenStore -> String -> IO ()
xsRemove xs = xstRemove xs emptyTransaction

xstRemove :: XenStore -> TransId -> String -> IO ()
xstRemove (XenStore commChan) tid k =
  standardRequest commChan tid (XSRm k) RTRm (const ())

xsSetPermissions :: XenStore -> String -> [XSPerm] -> IO ()
xsSetPermissions xs = xstSetPermissions xs emptyTransaction

xstSetPermissions :: XenStore -> TransId -> String -> [XSPerm] -> IO ()
xstSetPermissions (XenStore commChan) tid k ps =
  standardRequest commChan tid (XSSetPerms k ps) RTSetPerms (const ())

xsIsDomainIntroduced :: XenStore -> DomId -> IO Bool
xsIsDomainIntroduced xs = xstIsDomainIntroduced xs emptyTransaction

xstIsDomainIntroduced :: XenStore -> TransId -> DomId -> IO Bool
xstIsDomainIntroduced (XenStore commChan) tid d =
  standardRequest commChan tid (XSIsDomInt d) RTIsDomInt parseBool

xsResume :: XenStore -> DomId -> IO ()
xsResume xs = xstResume xs emptyTransaction

xstResume :: XenStore -> TransId -> DomId -> IO ()
xstResume (XenStore commChan) tid d =
  standardRequest commChan tid (XSResume d) RTResume (const ())

xsSetTarget :: XenStore -> DomId -> DomId -> IO ()
xsSetTarget xs = xstSetTarget xs emptyTransaction

xstSetTarget :: XenStore -> TransId -> DomId -> DomId -> IO ()
xstSetTarget (XenStore commChan) tid d td =
  standardRequest commChan tid (XSSetTarg d td) RTSetTarg (const ())

xsRestrict :: XenStore -> DomId -> IO ()
xsRestrict xs = xstRestrict xs emptyTransaction

xstRestrict :: XenStore -> TransId -> DomId -> IO ()
xstRestrict (XenStore commChan) tid d =
  standardRequest commChan tid (XSRestrict d) RTRestrict (const ())

#ifdef XS_RESET_WATCHES
xsResetWatches :: XenStore -> IO ()
xsResetWatches xs = xstResetWatches xs emptyTransaction

xstResetWatches :: XenStore -> TransId -> IO ()
xstResetWatches (XenStore commChan) tid =
  standardRequest commChan tid XSReset RTReset (const ())
#endif

standardRequest :: Chan DriverReq -> TransId ->
                   XenbusRequest -> ResponseType -> (ByteString -> a) ->
                   IO a
standardRequest commChan tid req goodresp converter = do
  respMV <- newEmptyMVar
  writeChan commChan (Request tid req respMV)
  processResponse goodresp converter =<< takeMVar respMV

-- ----------------------------------------------------------------------------

data DriverReq =
    Advance
  | WatchRequest String String (String -> String -> IO ()) (MVar ResponseBody)
  | Request TransId XenbusRequest (MVar ResponseBody)

data XenbusClientState = XenbusClientState {
    nextRequestId :: ReqId
  , waiterMap     :: Map ReqId (MVar ResponseBody)
  , watches       :: [(String, String -> String -> IO ())]
  , pendingReqs   :: [ByteString]
  , unparsedData  :: ByteString
  , shouldSignal  :: Bool
  }

initialClientState :: XenbusClientState
initialClientState = XenbusClientState {
    nextRequestId = ReqId 1
  , waiterMap     = Map.empty
  , watches       = []
  , pendingReqs   = []
  , unparsedData  = BS.empty
  , shouldSignal  = False
  }

updateState :: XenbusClientState -> DriverReq -> XenbusClientState
updateState st Advance               = st
updateState st (Request t r mv)      =
  let newreq = buildRequest (nextRequestId st) t r
  in st{ nextRequestId = advanceReqId (nextRequestId st)
       , waiterMap     = Map.insert (nextRequestId st) mv (waiterMap st)
       , pendingReqs   = pendingReqs st ++ [newreq]
       }
updateState st (WatchRequest s t v mv) =
  let watchreq = buildRequest (nextRequestId st) (TransId 0) (XSWatch s t)
  in st{ nextRequestId = advanceReqId (nextRequestId st)
       , waiterMap     = Map.insert (nextRequestId st) mv (waiterMap st)
       , watches       = (s, v) : watches st
       , pendingReqs   = pendingReqs st ++ [watchreq]
       }

addNewData :: XenbusClientState -> ByteString -> XenbusClientState
addNewData st bstr = st{ unparsedData = unparsedData st `BS.append` bstr
                       , shouldSignal = shouldSignal st || (not (BS.null bstr))}

processRawData :: XenbusClientState -> (XenbusClientState, [ResponseBody])
processRawData st =
  case runGetOrFail parseResponse (unparsedData st) of
    Left _ -> (st, [])
    Right (bytesLeft, _, x) ->
      let (st', rest) = processRawData st{ unparsedData = bytesLeft }
      in (st', x : rest)

processResponses :: XenbusClientState -> ResponseBody -> IO XenbusClientState
processResponses state (RespBody rid rtype bstr) =
  case Map.lookup rid (waiterMap state) of
    Just resmv -> do
      putMVar resmv (RespBody (ReqId 0) rtype bstr)
      return state{ waiterMap = Map.delete rid (waiterMap state) }
    Nothing    ->
      return state

processWatches :: XenbusClientState -> ResponseBody -> IO ()
processWatches state (RespBody _ RTEvent bstr) = do
  let bsparts = BS.split 0 bstr
      parts   = map (map (chr . fromIntegral) . BS.unpack) bsparts
  case parts of
    (key:token:_) ->
      forM_ (watches state) $ \ (watchOn, action) ->
        when (watchOn `isPrefixOf` key) $
          forkIO_ (action key token)
    _ ->
      writeDebugConsole "HaLVM/xenstore: Ignoring strange event notification.\n"
processWatches _ _ = return ()

writePendingRequests :: XenbusClientState -> XSRing -> IO XenbusClientState
writePendingRequests state ring =
  case pendingReqs state of
    []       -> return state
    (f:rest) -> do
      canDo <- canWriteReqData ring f
      if canDo
        then do let state' = state{ pendingReqs = rest, shouldSignal = True }
                writeNewReqData ring f >> writePendingRequests state' ring
        else return state

xenbusClient :: XSRing -> Port -> Chan DriverReq -> XenbusClientState -> IO ()
xenbusClient ring port commChan state0 = do
  state1 <- updateState state0 <$> readChan commChan
  state2 <- addNewData state1 <$> readNewRespData ring
  let (state3, resps) = processRawData state2
  state4 <- foldM processResponses state3 resps
  mapM_ (processWatches state4) resps
  state5 <- writePendingRequests state4 ring
  when (shouldSignal state5) $ sendOnPort port
  xenbusClient ring port commChan state5{ shouldSignal = False }

-- ----------------------------------------------------------------------------

buildRequest :: ReqId -> TransId -> XenbusRequest -> ByteString
buildRequest rid tid xbr = runPut $ do
  putWord32host (requestId xbr)                 -- uint32_t type
  putWord32host (unReqId rid)                   -- uint32_t req_id
  putWord32host (unTransId tid)                 -- uint32_t tx_id
  putWord32host (fromIntegral (BS.length body)) -- uint32_t len
  putLazyByteString body
 where body = runPut (renderBody xbr)

data ResponseBody = RespBody ReqId ResponseType ByteString

parseResponse :: Get ResponseBody
parseResponse = do
  rtype <- getResponseType <$> getWord32host
  rid   <- getWord32host
  _tid  <- getWord32host
  len   <- getWord32host
  body  <- getLazyByteString (fromIntegral len)
  return (RespBody (ReqId rid) rtype body)

-- ----------------------------------------------------------------------------

#include <stdint.h>
#include <sys/types.h>
#include <xen/io/xs_wire.h>

#ifndef XENSTORE_RING_SIZE
#error "BAD BAD BAD"
#endif

data ResponseType = RTRead     | RTWrite  | RTMkDir   | RTRm       | RTDir
                  | RTSetPerms | RTWatch  | RTUnwatch | RTRestrict | RTTransSt
                  | RTTransEnd | RTIntro  | RTRelease | RTGetPath  | RTError
                  | RTIsDomInt | RTResume | RTSetTarg | RTGetPerms | RTEvent
#ifdef XS_RESET_WATCHES
                  | RTReset
#endif
                  | RTUnknown Word32
 deriving (Eq)

getResponseType :: Word32 -> ResponseType
getResponseType (#const XS_DIRECTORY)            = RTDir
getResponseType (#const XS_READ)                 = RTRead
getResponseType (#const XS_GET_PERMS)            = RTGetPerms
getResponseType (#const XS_WATCH)                = RTWatch
getResponseType (#const XS_UNWATCH)              = RTUnwatch
getResponseType (#const XS_TRANSACTION_START)    = RTTransSt
getResponseType (#const XS_TRANSACTION_END)      = RTTransEnd
getResponseType (#const XS_INTRODUCE)            = RTIntro
getResponseType (#const XS_RELEASE)              = RTRelease
getResponseType (#const XS_GET_DOMAIN_PATH)      = RTGetPath
getResponseType (#const XS_WRITE)                = RTWrite
getResponseType (#const XS_MKDIR)                = RTMkDir
getResponseType (#const XS_RM)                   = RTRm
getResponseType (#const XS_SET_PERMS)            = RTSetPerms
getResponseType (#const XS_WATCH_EVENT)          = RTEvent
getResponseType (#const XS_ERROR)                = RTError
getResponseType (#const XS_IS_DOMAIN_INTRODUCED) = RTIsDomInt
getResponseType (#const XS_RESUME)               = RTResume
getResponseType (#const XS_SET_TARGET)           = RTSetTarg
getResponseType (#const XS_RESTRICT)             = RTRestrict
#ifdef XS_RESET_WATCHES
getResponseType (#const XS_RESET_WATCHES)        = RTReset
#endif
getResponseType x                                = RTUnknown x

processResponse :: ResponseType -> (ByteString -> a) -> ResponseBody -> IO a
processResponse goodtype converter (RespBody _ rtype body)
  | rtype == goodtype = return (converter body)
  | rtype == RTError  = throw (parseError body)
  | otherwise         = throw EIO

parseStrings :: ByteString -> [String]
parseStrings bstr = map translateAscii (BS.split 0 bstr)

parseString :: ByteString -> String
parseString bstr = translateAscii (BS.takeWhile (/= 0) bstr)

parseBool :: ByteString -> Bool
parseBool bstr = parseString bstr == "T"

parseTransId :: ByteString -> TransId
parseTransId bstr = TransId (read (parseString bstr))

parsePerms :: ByteString -> [XSPerm]
parsePerms bstr = map translateString (parseStrings bstr)
 where
  translateString ('r':rest) = ReadPerm      (toDomId (read rest :: Word16))
  translateString ('w':rest) = WritePerm     (toDomId (read rest :: Word16))
  translateString ('b':rest) = ReadWritePerm (toDomId (read rest :: Word16))
  translateString ('n':rest) = NonePerm      (toDomId (read rest :: Word16))
  translateString _          = throw EIO

translateAscii :: ByteString -> String
translateAscii  = map castCUCharToChar . map CUChar . BS.unpack

parseError :: ByteString -> ErrorCode
parseError bstr =
  case reads (parseString bstr) of
    [(x, [])] -> x
    _         -> EIO

-- ----------------------------------------------------------------------------

data XenbusRequest =
    XSRead     String
  | XSWrite    String String
  | XSMkDir    String
  | XSRm       String
  | XSDir      String
  | XSGetPerms String
  | XSSetPerms String [XSPerm]
  | XSWatch    String String
  | XSUnwatch  String
#ifdef XS_RESET_WATCHES
  | XSReset
#endif
  | XSTransSt
  | XSTransEnd Bool
  | XSIntro    DomId MFN Port
  | XSRelease  DomId
  | XSGetPath  DomId
  | XSIsDomInt DomId
  | XSResume   DomId
  | XSSetTarg  DomId DomId
  | XSRestrict DomId

requestId :: XenbusRequest -> Word32
requestId (XSRead     _)     = (#const XS_READ)
requestId (XSWrite    _ _)   = (#const XS_WRITE)
requestId (XSMkDir    _)     = (#const XS_MKDIR)
requestId (XSRm       _)     = (#const XS_RM)
requestId (XSDir      _)     = (#const XS_DIRECTORY)
requestId (XSGetPerms _)     = (#const XS_GET_PERMS)
requestId (XSSetPerms _ _)   = (#const XS_SET_PERMS)
requestId (XSWatch    _ _)   = (#const XS_WATCH)
requestId (XSUnwatch  _)     = (#const XS_UNWATCH)
#ifdef XS_RESET_WATCHES
requestId  XSReset           = (#const XS_RESET_WATCHES)
#endif
requestId  XSTransSt         = (#const XS_TRANSACTION_START)
requestId (XSTransEnd _)     = (#const XS_TRANSACTION_END)
requestId (XSIntro    _ _ _) = (#const XS_INTRODUCE)
requestId (XSRelease  _)     = (#const XS_RELEASE)
requestId (XSGetPath  _)     = (#const XS_GET_DOMAIN_PATH)
requestId (XSIsDomInt _)     = (#const XS_IS_DOMAIN_INTRODUCED)
requestId (XSResume   _)     = (#const XS_RESUME)
requestId (XSSetTarg  _ _)   = (#const XS_SET_TARGET)
requestId (XSRestrict _)     = (#const XS_RESTRICT)

renderBody :: XenbusRequest -> Put
renderBody (XSRead     str)          =
  renderStr str >> addNull
renderBody (XSWrite    key  val)     =
  renderStr key >> addNull >> renderStr val
renderBody (XSMkDir    str)          =
  renderStr str >> addNull
renderBody (XSRm       str)          =
  renderStr str >> addNull
renderBody (XSDir      str)          =
  renderStr str >> addNull
renderBody (XSGetPerms str)          =
  renderStr str >> addNull
renderBody (XSSetPerms str  perms)   =
  renderStr str >> addNull >> renderPerms perms
renderBody (XSWatch    str  token)   =
  renderStr str >> addNull >> renderStr token >> addNull
renderBody (XSUnwatch  str)          =
  renderStr str >> addNull
#ifdef XS_RESET_WATCHES
renderBody  XSReset                  =
  addNull
#endif
renderBody  XSTransSt                =
  addNull
renderBody (XSTransEnd good)         =
  renderStr (if good then "T" else "F") >> addNull
renderBody (XSIntro    dom  mfn prt) =
  renderDom dom  >> addNull >>
  renderMFN mfn  >> addNull >>
  renderPort prt >> addNull
renderBody (XSRelease  dom)          =
  renderDom dom >> addNull
renderBody (XSGetPath  dom)          =
  renderDom dom >> addNull
renderBody (XSIsDomInt dom)          =
  renderDom dom >> addNull
renderBody (XSResume   dom)          =
  renderDom dom >> addNull
renderBody (XSSetTarg  dom  tdom)    =
  renderDom dom >> addNull >> renderDom tdom >> addNull
renderBody (XSRestrict dom)          =
  renderDom dom >> addNull

renderStr :: String -> Put
renderStr str = mapM_ putWord8 (map (unCUChar . castCharToCUChar) str)
 where unCUChar (CUChar x) = x

renderPerms :: [XSPerm] -> Put
renderPerms  = mapM_ (\ p -> renderPerm p >> addNull)

renderPerm :: XSPerm -> Put
renderPerm  (WritePerm     d) = renderStr "w" >> renderDom d
renderPerm  (ReadPerm      d) = renderStr "r" >> renderDom d
renderPerm  (ReadWritePerm d) = renderStr "b" >> renderDom d
renderPerm  (NonePerm      d) = renderStr "n" >> renderDom d

renderDom :: DomId -> Put
renderDom d = renderStr (show (fromDomId d :: Word16))

renderMFN :: MFN -> Put
renderMFN f = renderStr (show (fromMFN f))

renderPort :: Port -> Put
renderPort p = renderStr (show (fromPort p :: Word32))

addNull :: Put
addNull = putWord8 0

-- ----------------------------------------------------------------------------

readNewRespData :: XSRing -> IO ByteString
readNewRespData ring = BS.pack <$> readBytes
 where
  readBytes :: IO [Word8]
  readBytes = do
    mnext <- nextByte
    case mnext of
      Nothing -> return []
      Just x  -> (x:) <$> readBytes
  --
  nextByte :: IO (Maybe Word8)
  nextByte = do
    cons <- respConsumed ring
    prod <- respProduced ring
    assert (prod >= cons) $ return ()
    if cons == prod
      then return Nothing
      else do byte <- peekByteOff (responseRing ring)
                                  (fromIntegral cons `mod` respRingSize)
              setRespConsumed ring (cons + 1)
              return (Just byte)

canWriteReqData :: XSRing -> ByteString -> IO Bool
canWriteReqData ring bstr = do
  prod <- reqConsumed ring
  cons <- reqProduced ring
  return (((prod + amt) - cons) <= reqRingSize)
 where amt = fromIntegral (BS.length bstr)

writeNewReqData :: XSRing -> ByteString -> IO ()
writeNewReqData ring bstr = do
  sane <- canWriteReqData ring bstr
  assert sane $ return ()
  writeBytes (BS.unpack bstr)
 where
  writeBytes []    = return ()
  writeBytes (f:r) = do
    prod <- reqProduced ring
    pokeByteOff (requestRing ring) (fromIntegral prod `mod` reqRingSize) f
    setReqProduced ring (prod + 1)
    writeBytes r

-- ----------------------------------------------------------------------------

type XSRing = Ptr Word8

reqRingSize :: Integral a => a
reqRingSize  = 1024

respRingSize :: Integral a => a
respRingSize  = 1024

requestRing :: XSRing -> Ptr Word8
requestRing ring = ring `plusPtr` (#offset struct xenstore_domain_interface,req)

responseRing :: XSRing -> Ptr Word8
responseRing r = r `plusPtr` (#offset struct xenstore_domain_interface,rsp)

reqConsumed :: XSRing -> IO Word32
reqConsumed ring = (#peek struct xenstore_domain_interface,req_cons) ring

reqProduced :: XSRing -> IO Word32
reqProduced ring = (#peek struct xenstore_domain_interface,req_prod) ring

setReqProduced :: XSRing -> Word32 -> IO ()
setReqProduced ring v = (#poke struct xenstore_domain_interface,req_prod) ring v

respConsumed :: XSRing -> IO Word32
respConsumed ring = (#peek struct xenstore_domain_interface,rsp_cons) ring

setRespConsumed :: XSRing -> Word32 -> IO ()
setRespConsumed r v = (#poke struct xenstore_domain_interface,rsp_cons) r v

respProduced :: XSRing -> IO Word32
respProduced r = (#peek struct xenstore_domain_interface,rsp_prod) r

-- ----------------------------------------------------------------------------

forkIO_ :: IO () -> IO ()
forkIO_ m = forkIO m >> return ()

-- ----------------------------------------------------------------------------

foreign import ccall unsafe "domain_info.h get_xenstore_evtchn"
  get_xenstore_evtchn :: IO Word32

foreign import ccall unsafe "domain_info.h get_xenstore_mfn"
  get_xenstore_mfn :: IO Word


