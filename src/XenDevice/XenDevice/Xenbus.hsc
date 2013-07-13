-- blank line for Haddock/hsc2hs
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>, 
-- BANNEREND
-- |Client support for Xenbus and, more importantly, the XenStore.
module XenDevice.Xenbus(dXenbus, dXenbus',
                        XBResult(..), Permissions, PermissionOps(..),
                        xsDirectory, xsMkDir, 
                        xsRead, xsWrite,
                        xsRm,
                        WatchId,
                        xsSetWatch, xsUnsetWatch, xsIntroduce, xsRelease,
                        getStoreMfn, getStoreEvtChn, xsSetPermissions,
			myDomId)
    where

import XenDevice.XenbusInterface(Interface(..), interface, IdxPtr, canWrite, canRead, xbWrite, xbRead)
import Hypervisor.Basics
import Hypervisor.Debug
import XenDevice.Codecs 
import Hypervisor.Port
import Hypervisor.Memory(mfnToVPtr, VPtr, MFN, toMFN, mapForeignMachineFrames)
import Foreign.C.Types(CChar)
import Foreign.Marshal.Alloc(free,mallocBytes)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.IORef
import Control.Monad
import Control.Concurrent
import GHC.IO(unsafePerformIO)
import Hypervisor.BufferChan
import Hypervisor.Kernel

#include <errno.h>
#include "types.h"
#include "xen/io/xs_wire.h"

-- |The return type used for most XenBus\/XenStore operations.
data XBResult a =
   XBOk a -- ^ The operation succeeded
 | XBError String -- ^ The operation failed
    deriving Show


-- |Permissions for a node: owning domain, default permissions, and
-- access control list.
type Permissions = ((DomId,PermissionOps), [(DomId, PermissionOps)])

-- |Permissible operations.  if 'readOp', the node can be read.  if
-- 'writeOp', the node can be written.

data PermissionOps = PermissionOps { readOp :: Bool
                                   , writeOp :: Bool
                                   }

newtype WatchId = WatchId String

-- |The definition of the Xenbus\/Xenstore device driver. If you intend to use
-- any of the functions below, you should include this in the list of drivers
-- passed to halvm_kernel or halvm_kernel_daemon.
dXenbus :: DeviceDriver
dXenbus = driver initXenbus

-- | A version of 'dXenbus' which allows you to connect to some non-standard XenStore.
dXenbus' :: VPtr a -> Port -> DeviceDriver
dXenbus' page port = driver (initXenbus' page port)

driver :: IO () -> DeviceDriver
driver i = DeviceDriver { devName = "XenDeviceInternalXenbus"
                        , dependencies = []
                        , initialize = i
                        , shutdown = return ()
                        }

-- |Return the current domain's DomId by looking it up in the Xenstore.
myDomId :: IO DomId
myDomId = do
  res <- xsRead "domid"
  case res of
    XBError _    -> xThrow ENOSYS
    XBOk    res' ->
      case reads res' of
	[(domid, "")] -> return (DomId domid)
	_	      -> xThrow EPROTO
	

-- |The XenStore equivalent of the "ls" command. Returns the relative path of
-- every key in the directory upon success.
xsDirectory :: String -> IO (XBResult [String])
xsDirectory path = talk (#const XS_DIRECTORY) 0 0  [path] encodeBody0 decodeMultiBody

-- |Read the value from the given path.
xsRead :: String -> IO (XBResult String)
xsRead path = talk (#const XS_READ) 0 0 [path] encodeBody0 decodeSingleBody

-- |Make a directory with the given path.
xsMkDir :: String -> IO (XBResult ())
xsMkDir path = talk (#const XS_MKDIR) 0 0 [path] encodeBody0 ignoreRest

-- |Set permissions at the given path.
xsSetPermissions :: String -> Permissions -> IO (XBResult ())
xsSetPermissions path (ownerdef,acl) =
  talk (#const XS_SET_PERMS) 0 0 ([path]++map showAc (ownerdef:acl)) 
       encodeBody0 ignoreRest
  where showAc (DomId d,ops) = showOp (readOp ops,writeOp ops):show d
        showOp (True, False) =  'r'
        showOp (False,True)  =  'w'
        showOp (True, True)  =  'b'
        showOp (False,False) =  'n'

-- |Set the value at the given path.
xsWrite :: String -> String -> IO (XBResult ())
xsWrite path value = talk (#const XS_WRITE) 0 0 [path,value] encodeBody ignoreRest

-- |Remove the value\/directory at the given path. The remove is recursive in the
-- case of directories.
xsRm :: String -> IO(XBResult ())
xsRm path = talk (#const XS_RM) 0 0 [path] encodeBody0 ignoreRest

-- |Set a watch on the given path so that the given function is invoked whenever
-- that path (or its sub-paths) is modified.
xsSetWatch :: String -> (WatchId -> String -> IO ()) -> IO (XBResult WatchId)
xsSetWatch path callback = 
    do wid@(WatchId idStr) <- nextWatchId
       res <- talk (#const XS_WATCH) 0 0 [path,idStr] encodeBody0 ignoreRest
       case res of
         XBOk _ -> do watches <- takeMVar xsWatches
                      putMVar xsWatches $ (wid,callback):watches
                      return $ XBOk wid
         XBError e -> return $ XBError e

-- |Remove the given watch.
xsUnsetWatch :: WatchId -> IO (XBResult ())
xsUnsetWatch (WatchId idStr) =
    do watches <- takeMVar xsWatches
       putMVar xsWatches $ removeWatch watches
       talk (#const XS_UNWATCH) 0 0 [idStr] encodeBody0 ignoreRest
    where removeWatch [] = []
          removeWatch (((WatchId fidStr), _):rest) | fidStr == idStr = (removeWatch rest)
          removeWatch (first:rest) = first:(removeWatch rest)

-- |Introduce a new domain by its id, an MFN, and a port.
xsIntroduce :: DomId -> MFN -> Port -> IO (XBResult ())
xsIntroduce (DomId d) mfn p =
  do talk (#const XS_INTRODUCE) 0 0 [show d, show' mfn, show' p] 
          encodeBody0 ignoreRest
 where 
  show' :: Show a => a -> String
  show' = drop 1 . dropWhile (/= ' ') . show

-- |Release a domain given its id.
xsRelease :: DomId -> IO (XBResult ())
xsRelease (DomId d) =
  do talk (#const XS_RELEASE) 0 0 [show d] encodeBody0 ignoreRest

-----------------------------------------------------------
-- private implementation follows
-----------------------------------------------------------

-- some simple talking in the format. can spin.

-- do a	simple-minded roundtrip.  assumes replies are paired neatly with requests
talk :: SockMsgType -> ReqId -> TxId -> [String] -> Encoder -> Decoder e a  -> IO (XBResult a)
talk msgType reqId txId fields requestEncoder replyDecoder =
   do writeMessage msgType reqId txId (requestEncoder fields)
      (msgType',reqId',txId',reply) <- readMessage
      case msgType' of
        (#const XS_ERROR) -> liftM XBError $ uncheckedDecode decodeSingleBody0 reply
        _ -> if (msgType /= msgType' || reqId /= reqId' || txId /= txId') then
               let err_msg = "reply mismatch:" ++ (show msgType') ++ " " ++ (show reqId') ++ " " ++ (show txId') ++ "\n" in
               do writeDebugConsole ("talkXenbus " ++ err_msg)
                  return $ XBError err_msg
             else
               liftM XBOk $ uncheckedDecode replyDecoder reply

-- write a single message
writeMessage :: SockMsgType -> ReqId -> TxId -> [CChar] -> IO ()
writeMessage msgType reqId txId body =
  do (len,fp) <- encodeMessage msgType reqId txId body
     withForeignPtr fp $ \p -> writeRaw p len
   
readMessage :: IO XBMessage
readMessage =
    do chan <- readIORef xsReaderChan
       readChan chan

readMessageFromChan :: Chan CChar -> IO XBMessage
readMessageFromChan inChan = readMessageInternal read'
  where read' len = do fp <- mallocForeignPtrArray len 
                       withForeignPtr fp $ \p -> readRaw inChan p len
                       return (len,fp)


-- Original, spinning version of read and write.
-- If we stuck brief threadDelays into the spin loops
-- this would probably work just fine.  But eventually
-- we'll want to Watch for things, and we'll need to use
-- the event channel to do that...

{-
writeRaw :: Ptr Word8 -> Int -> IO ()
writeRaw buff len =
   do sent <- xb_write buff len
      let len' = len - sent
      if len' > 0 
       then
         writeRaw (buff `plusPtr` sent) len'
       else
         return ()

readRaw :: Ptr Word8 -> Int -> IO ()
readRaw buff len =
   do received <- xb_read buff len
      let len' = len - received
      if len' > 0 
       then
         readRaw (buff `plusPtr` received) len'
       else
         return ()

-}

-- A non-blocking implementation of the Raw operators.
-- All the coercions between buffers and channel elements are pretty
-- inefficient, but allowed a plug-compatible replacement for the ones above.

{-# LANGUAGE xsReaderChan #-}
xsReaderChan :: IORef (Chan XBMessage)
xsReaderChan = unsafePerformIO $ newChan >>= newIORef

{-# LANGUAGE xsWriterChan #-}
xsWriterChan :: MVar (Chan [CChar])
xsWriterChan = unsafePerformIO newEmptyMVar

{-# LANGUAGE xsWatches #-}
xsWatches :: MVar [(WatchId, WatchId -> String -> IO ())]
xsWatches = unsafePerformIO $ newMVar []

{-# LANGUAGE xsNextWatchId #-}
xsNextWatchId :: MVar Word32
xsNextWatchId = unsafePerformIO $ newMVar 25

nextWatchId :: IO WatchId
nextWatchId =
    do cur <- takeMVar xsNextWatchId
       putMVar xsNextWatchId $ cur + 1
       return $ WatchId (show cur)

-- eventually, we may want support for multiple xenstore connections,
-- then the global variables above need to go into the xenstore context.

initXenbus :: IO ()
initXenbus =
  do storeMFN <- getStoreMfn
     page <- mfnToVPtr storeMFN `xCatch` (\ _ -> do
               mapForeignMachineFrames domidSelf [storeMFN])
     port <- toPort `fmap` get_store_evtchn
     initXenbus' page port

initXenbus' :: VPtr a -> Port -> IO ()
initXenbus' page port =
  do i <- interface page port
     internalChan <- mkReaderChan (readRaw' (rsp_prodp i) (rsp_consp i) (rsp_shdata i) port) 
                                  (canRead (rsp_prodp i) (rsp_consp i)) (waitSet i) port 
     mkWriterChan (writeRaw' (req_prodp i) (req_consp i) (req_shdata i) port) 
                  (canWrite (req_prodp i) (req_consp i)) (waitSet i) port >>= putMVar xsWriterChan
     readerChan <- readIORef xsReaderChan
     forkIO_ $ translatorThread internalChan readerChan

translatorThread :: Chan CChar -> Chan XBMessage -> IO ()
translatorThread inChan outChan =
    do msg@(msgType, _, _, body) <- readMessageFromChan inChan
       if msgType == (#const XS_WATCH_EVENT)
          then do [path, wid] <- uncheckedDecode decodeMultiBody body
                  forkIO_ $ triggerWatch wid path
          else writeChan outChan msg
       translatorThread inChan outChan

triggerWatch :: String -> String -> IO ()
triggerWatch idStr path =
    do watches <- takeMVar xsWatches
       mapM_ (\(wid@(WatchId curIdStr),callback) ->
                  when (curIdStr == idStr) $ forkIO_ $ callback wid path)
             watches
       putMVar xsWatches watches

writeRaw :: Ptr CChar -> Int -> IO ()
writeRaw buff len =
  do chan <- readMVar xsWriterChan
     peekArray len buff >>= writeChan chan

readRaw :: Chan CChar -> Ptr CChar -> Int -> IO ()
readRaw chan buff len =
  sequence (replicate len (readChan chan)) >>= pokeArray buff

readRaw' :: IdxPtr -> IdxPtr -> Ptr CChar -> Port -> IO [CChar]
readRaw' prodp consp shdata port = do
  writeDebugConsole "Starting readRaw'\n"
  let maxSize = (#const XENSTORE_RING_SIZE)
--  writeDebugConsole ("Allocating array with max size: " ++ show maxSize ++ "\n")
--  arr <- mallocArray maxSize
  base <- mallocBytes 16384
  let arr = castPtr (base `plusPtr` 4096)
  len <- xbRead prodp consp shdata port arr maxSize
  res <- peekArray len arr
  free base
  writeDebugConsole "Ending readRaw'\n"
  return res

writeRaw' :: IdxPtr -> IdxPtr -> Ptr CChar -> Port -> [CChar] -> IO Int
writeRaw' prodp consp shdata port s =
  withArray s  $ \a -> xbWrite prodp consp shdata port a (length s)

getStoreMfn :: IO MFN
getStoreMfn = (toMFN . fromIntegral) `fmap` get_store_mfn

getStoreEvtChn :: IO Port
getStoreEvtChn = toPort `fmap` get_store_evtchn

forkIO_ :: IO () -> IO ()
forkIO_ body =
    do _ <- forkIO body
       return ()

foreign import ccall unsafe "xenbus.h get_store_evtchn"
  get_store_evtchn :: IO Word32
foreign import ccall unsafe "xenbus.h get_store_mfn"
  get_store_mfn :: IO Word

