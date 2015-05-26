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
       , xsGetDomId, xsGetDomName
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
import Data.Bits
import Data.ByteString(packCStringLen,useAsCStringLen)
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
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
import Text.Read (readMaybe)

newtype XenStore = XenStore (MVar XenbusState)

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
  xsPtr <- handle (mapMFN xsMFN) (mfnToVPtr xsMFN)
  let initState = state0 xsPort xsPtr
  stateMV <- newMVar initState
  setPortHandler xsPort (onXenbusEvent stateMV)
  return (XenStore stateMV)
 where
  mapMFN :: MFN -> ErrorCode -> IO (VPtr a)
  mapMFN mfn _ = mapFrames [mfn]

xsGetDomId :: XenStore -> IO DomId
xsGetDomId xs = do
  val <- xsRead xs "domid"
  maybe (throwIO EPROTO) (return . toDomId) (readMaybe val :: Maybe Word16)

xsGetDomName :: XenStore -> IO String
xsGetDomName xs = xsRead xs "name"

xsDirectory :: XenStore -> String -> IO [String]
xsDirectory xs = xstDirectory xs emptyTransaction

xstDirectory :: XenStore -> TransId -> String -> IO [String]
xstDirectory (XenStore xbs) tid str =
  standardRequest xbs tid (XSDir str) RTDir parseStrings

xsRead :: XenStore -> String -> IO String
xsRead xs = xstRead xs emptyTransaction

xstRead :: XenStore -> TransId -> String -> IO String
xstRead (XenStore xbs) tid str =
  standardRequest xbs tid (XSRead str) RTRead parseString

xsGetPermissions :: XenStore -> String -> IO [XSPerm]
xsGetPermissions xs = xstGetPermissions xs emptyTransaction

xstGetPermissions :: XenStore -> TransId -> String -> IO [XSPerm]
xstGetPermissions (XenStore xbs) tid str =
  standardRequest xbs tid (XSGetPerms str) RTGetPerms parsePerms

xsWatch :: XenStore -> String -> String -> (String -> String -> IO ()) -> IO ()
xsWatch (XenStore xbs) str token handler =
  do modifyMVar_ xbs $ \ state ->
       let oldwatches = waitingWatches state
       in return state{ waitingWatches = oldwatches ++ [(str, token, handler)] }
     standardRequest xbs emptyTransaction (XSWatch str token) RTWatch (const ())

xsUnwatch :: XenStore -> String -> String -> IO ()
xsUnwatch xs = xstUnwatch xs emptyTransaction

xstUnwatch :: XenStore -> TransId -> String -> String -> IO ()
xstUnwatch (XenStore xbs) tid wpth tok =
  do modifyMVar_ xbs $ \ state ->
       let watches = filter (\ (a,b,_) -> (a /= wpth) || (b /= tok))
                            (waitingWatches state)
       in return state{ waitingWatches = watches }
     standardRequest xbs tid (XSUnwatch wpth tok) RTUnwatch (const ())

xsStartTransaction :: XenStore -> IO TransId
xsStartTransaction xs = xstStartTransaction xs emptyTransaction

xstStartTransaction :: XenStore -> TransId -> IO TransId
xstStartTransaction (XenStore xbs) tid =
  standardRequest xbs tid XSTransSt RTTransSt parseTransId

xstAbort :: XenStore -> TransId -> IO ()
xstAbort (XenStore xbs) tid =
  standardRequest xbs tid (XSTransEnd False) RTTransEnd (const ())

xstCommit :: XenStore -> TransId -> IO ()
xstCommit (XenStore xbs) tid =
  standardRequest xbs tid (XSTransEnd True) RTTransEnd (const ())

xsIntroduce :: XenStore -> DomId -> MFN -> Port -> IO ()
xsIntroduce xs = xstIntroduce xs emptyTransaction

xstIntroduce :: XenStore -> TransId -> DomId -> MFN -> Port -> IO ()
xstIntroduce (XenStore xbs) tid d m p =
  standardRequest xbs tid (XSIntro d m p) RTIntro (const ())

xsRelease :: XenStore -> DomId -> IO ()
xsRelease xs = xstRelease xs emptyTransaction

xstRelease :: XenStore -> TransId -> DomId -> IO ()
xstRelease (XenStore xbs) tid d =
  standardRequest xbs tid (XSRelease d) RTRelease (const ())

xsGetDomainPath :: XenStore -> DomId -> IO String
xsGetDomainPath xs = xstGetDomainPath xs emptyTransaction

xstGetDomainPath :: XenStore -> TransId -> DomId -> IO String
xstGetDomainPath (XenStore xbs) tid d =
  standardRequest xbs tid (XSGetPath d) RTGetPath parseString

xsWrite :: XenStore -> String -> String -> IO ()
xsWrite xs = xstWrite xs emptyTransaction

xstWrite :: XenStore -> TransId -> String -> String -> IO ()
xstWrite (XenStore xbs) tid k v =
  standardRequest xbs tid (XSWrite k v) RTWrite (const ())

xsMakeDirectory :: XenStore -> String -> IO ()
xsMakeDirectory xs = xstMakeDirectory xs emptyTransaction

xstMakeDirectory :: XenStore -> TransId -> String -> IO ()
xstMakeDirectory (XenStore xbs) tid d =
  standardRequest xbs tid (XSMkDir d) RTMkDir (const ())

xsRemove :: XenStore -> String -> IO ()
xsRemove xs = xstRemove xs emptyTransaction

xstRemove :: XenStore -> TransId -> String -> IO ()
xstRemove (XenStore xbs) tid k =
  standardRequest xbs tid (XSRm k) RTRm (const ())

xsSetPermissions :: XenStore -> String -> [XSPerm] -> IO ()
xsSetPermissions xs = xstSetPermissions xs emptyTransaction

xstSetPermissions :: XenStore -> TransId -> String -> [XSPerm] -> IO ()
xstSetPermissions (XenStore xbs) tid k ps =
  standardRequest xbs tid (XSSetPerms k ps) RTSetPerms (const ())

xsIsDomainIntroduced :: XenStore -> DomId -> IO Bool
xsIsDomainIntroduced xs = xstIsDomainIntroduced xs emptyTransaction

xstIsDomainIntroduced :: XenStore -> TransId -> DomId -> IO Bool
xstIsDomainIntroduced (XenStore xbs) tid d =
  standardRequest xbs tid (XSIsDomInt d) RTIsDomInt parseBool

xsResume :: XenStore -> DomId -> IO ()
xsResume xs = xstResume xs emptyTransaction

xstResume :: XenStore -> TransId -> DomId -> IO ()
xstResume (XenStore xbs) tid d =
  standardRequest xbs tid (XSResume d) RTResume (const ())

xsSetTarget :: XenStore -> DomId -> DomId -> IO ()
xsSetTarget xs = xstSetTarget xs emptyTransaction

xstSetTarget :: XenStore -> TransId -> DomId -> DomId -> IO ()
xstSetTarget (XenStore xbs) tid d td =
  standardRequest xbs tid (XSSetTarg d td) RTSetTarg (const ())

xsRestrict :: XenStore -> DomId -> IO ()
xsRestrict xs = xstRestrict xs emptyTransaction

xstRestrict :: XenStore -> TransId -> DomId -> IO ()
xstRestrict (XenStore xbs) tid d =
  standardRequest xbs tid (XSRestrict d) RTRestrict (const ())

#ifdef XS_RESET_WATCHES
xsResetWatches :: XenStore -> IO ()
xsResetWatches xs = xstResetWatches xs emptyTransaction

xstResetWatches :: XenStore -> TransId -> IO ()
xstResetWatches (XenStore xbs) tid =
  standardRequest xbs tid XSReset RTReset (const ())
#endif

standardRequest :: MVar XenbusState ->
                   TransId -> XenbusRequest ->
                   ResponseType -> (ByteString -> a) ->
                   IO a
standardRequest stateMV tid req goodresp converter =
  writeRequest stateMV tid req $ \ body ->
    case body of
      RespBody _ RTError bstr ->
        case reads (parseString bstr) of
          ((x,_):_)  -> throwIO (x :: ErrorCode)
          _          -> throwIO EIO
      RespBody _ rtype bstr | rtype == goodresp ->
        return (converter bstr)
      RespBody _ rtype _ ->
        do writeDebugConsole ("ERROR: Xenbus: Expected " ++ show goodresp ++
                              " but got " ++ show rtype ++ "\n")
           throwIO EIO

-- ----------------------------------------------------------------------------

data XenbusState = XBS {
    xbPort          :: Port
  , xbRing          :: XSRing
  , nextRequestId   :: ReqId
  , decodeStream    :: ByteString
  , pendingWrites   :: ByteString
  , waitingRequests :: Map ReqId (MVar ResponseBody)
  , waitingWatches  :: [(String, String, String -> String -> IO ())]
  }

state0 :: Port -> XSRing -> XenbusState
state0 port ring = XBS {
    xbPort          = port
  , xbRing          = ring
  , nextRequestId   = ReqId 1000
  , decodeStream    = BS.empty
  , pendingWrites   = BS.empty
  , waitingRequests = Map.empty
  , waitingWatches  = []
  }

writeRequest :: MVar XenbusState ->
                TransId -> XenbusRequest ->
                (ResponseBody -> IO a) ->
                IO a
writeRequest stateMV tid req k =
  do respMV <- newEmptyMVar
     state <- takeMVar stateMV
     let rid     = nextRequestId state
         newbstr = buildRequest rid tid req
         pend    = pendingWrites state `BS.append` newbstr
         table'  = Map.insert rid respMV (waitingRequests state)
     remain <- writeRequestData (xbPort state) (xbRing state) pend
     putMVar stateMV $! state{ nextRequestId   = advanceReqId rid
                             , pendingWrites   = remain
                             , waitingRequests = table' }
     takeMVar respMV >>= k

onXenbusEvent :: MVar XenbusState -> IO ()
onXenbusEvent stateMV =
  do state <- takeMVar stateMV
     let port = xbPort state
         ring = xbRing state
     inputbstr <- readNewResponseData port ring (decodeStream state)
     let (resps, inputbstr') = parseResponses inputbstr
     tab <- foldM' (waitingRequests state) resps $ \ table resp ->
       case resp of
         -- Fire any watches associated with this event
         RespBody _ RTEvent bstr ->
           do let bsparts = BS.split 0 bstr
                  parts   = map (map (chr . fromIntegral) . BS.unpack) bsparts
                  (key:token:_) = parts
              forM_ (waitingWatches state) $ \ (watchOn, _, action) ->
                when (watchOn `isPrefixOf` key) $
                  forkIO_ (action key token)
              return table
         -- Run any handlers associated with this id
         RespBody rid _ _ ->
           case Map.lookup rid table of
             Nothing ->
               do writeDebugConsole ("WARNING: Xenbus: Response with bad id: "
                                      ++ (show rid) ++ "\n")
                  return table
             Just mvar ->
               do putMVar mvar resp
                  return (Map.delete rid table)
     remn <- writeRequestData port ring (pendingWrites state)
     putMVar stateMV $! state{ decodeStream = inputbstr'
                             , pendingWrites = remn
                             , waitingRequests = tab }
 where
  readNewResponseData port ring acc =
    do newstuff <- readResponseData port ring
       if BS.null newstuff
         then return acc
         else readNewResponseData port ring (acc `BS.append` newstuff)

parseResponses :: ByteString -> ([ResponseBody], ByteString)
parseResponses bstr =
  case runGetOrFail parseResponse bstr of
    Left  (_, _, _)   -> ([], bstr)
    Right (rest,   _, req) ->
      let (res, remain) = parseResponses rest
      in (req : res, remain)

buildRequest :: ReqId -> TransId -> XenbusRequest -> ByteString
buildRequest rid tid xbr = runPut $ do
  putWord32host (requestId xbr)                 -- uint32_t type
  putWord32host (unReqId rid)                   -- uint32_t req_id
  putWord32host (unTransId tid)                 -- uint32_t tx_id
  putWord32host (fromIntegral (BS.length body)) -- uint32_t len
  putLazyByteString body
 where body = runPut (renderBody xbr)


-- ----------------------------------------------------------------------------

#include <stdint.h>
#include <sys/types.h>
#include <xen/io/xs_wire.h>

#ifndef XENSTORE_RING_SIZE
#error "BAD BAD BAD"
#endif

data ResponseBody = RespBody ReqId ResponseType ByteString

parseResponse :: Get ResponseBody
parseResponse =
  do rtype <- getResponseType <$> getWord32host
     rid   <- getWord32host
     _tid  <- getWord32host
     len   <- getWord32host
     body  <- getLazyByteString (fromIntegral len)
     return (RespBody (ReqId rid) rtype body)

data ResponseType = RTRead     | RTWrite  | RTMkDir   | RTRm       | RTDir
                  | RTSetPerms | RTWatch  | RTUnwatch | RTRestrict | RTTransSt
                  | RTTransEnd | RTIntro  | RTRelease | RTGetPath  | RTError
                  | RTIsDomInt | RTResume | RTSetTarg | RTGetPerms | RTEvent
#ifdef XS_RESET_WATCHES
                  | RTReset
#endif
                  | RTUnknown Word32
 deriving (Show, Eq)

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
  | XSUnwatch  String String
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
requestId (XSUnwatch  _ _)   = (#const XS_UNWATCH)
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
renderBody (XSUnwatch  str  token)   =
  renderStr str >> addNull >> renderStr token >> addNull
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

writeRequestData :: Port -> XSRing -> ByteString -> IO ByteString
writeRequestData port ring bstr =
  do cons <- reqConsumed ring
     prod <- reqProduced ring
     systemMB
     when ((prod - cons) > reqRingSize) $ fail "XenStore invariant broke."
     let (buf, len) = getOutputChunk cons prod
     let (writeBuf, leftBuf) = BS.splitAt (fromIntegral len) bstr
     let len' = min (fromIntegral len) (fromIntegral (BS.length writeBuf))
     unless (len == 0) $
       do writeBS writeBuf buf
          systemWMB
          setReqProduced ring (prod + len')
          sendOnPort port
     return leftBuf
 where
  getOutputChunk cons prod =
    let prodMask = prod .&. (reqRingSize - 1)
        maxLen   = reqRingSize - prodMask
        roomLeft = reqRingSize - (prod - cons)
        len      = if roomLeft < maxLen then roomLeft else maxLen
    in (requestRing ring `plusPtr` fromIntegral prodMask, len)

readResponseData :: Port -> XSRing -> IO ByteString
readResponseData port ring = handle printError $
  do cons <- respConsumed ring
     prod <- respProduced ring
     systemMB
     when ((prod - cons) > respRingSize) $ fail "XenStore invariant broke."
     let (buf, len) = getInputChunk cons prod
     if len == 0
        then return BS.empty
        else do systemRMB
                bstr <- packCStringLen (buf, fromIntegral len)
                systemMB
                setRespConsumed ring (cons + len)
                sendOnPort port
                return (BS.fromStrict bstr)
 where
  printError :: SomeException -> IO ByteString
  printError e =
   do writeDebugConsole ("Xenbus: Caught exception: " ++ show e ++ "\n") 
      return BS.empty
  getInputChunk cons prod =
    let consMask = cons .&. (respRingSize - 1)
        maxLen   = respRingSize - consMask
        len      = if (prod - cons) < maxLen then prod - cons else maxLen
    in (responseRing ring `plusPtr` fromIntegral consMask, len)

-- ----------------------------------------------------------------------------

type XSRing = Ptr Word8

reqRingSize :: Word32
reqRingSize  = 1024

respRingSize :: Word32
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

writeBS :: ByteString -> Ptr a -> IO ()
writeBS bstr buf = go (BS.toChunks bstr) buf
 where
  go [] _ = return ()
  go (f:rest) p = useAsCStringLen f $
    \ (ptr, len) ->
      do memcpy p (castPtr ptr) len
         go rest (p `plusPtr` len)

foldM' :: a -> [b] -> (a -> b -> IO a) -> IO a
foldM' val0 ls f = foldM f val0 ls 

-- ----------------------------------------------------------------------------

foreign import ccall unsafe "domain_info.h get_xenstore_evtchn"
  get_xenstore_evtchn :: IO Word32

foreign import ccall unsafe "domain_info.h get_xenstore_mfn"
  get_xenstore_mfn :: IO Word

foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr a -> Int -> IO ()

