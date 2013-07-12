-- Copyright 2013, Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |Routines for automatically performing rendezvous between two domains.
module Communication.Rendezvous(
         RendezvousCapable(..)
       , peerConnection
       , clientServerConnection
       )
 where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Memory
import Hypervisor.Port
import Hypervisor.XenStore

-- |The class of objects that are connectable in a peer-to-peer fashion.
-- If your underlying system (whatever it may be) uses an interface like this,
-- then this library can automatically set up connection rendezvous for you
-- through the XenStore.
--
-- The first type is an "extra" bit of information that is useful to the
-- system.
--
-- The second type is the thing the "accepting" side will receive, the third
-- is the type of thing the "offering" side will receive.
class (Show a, Read a) => RendezvousCapable a b c | b c -> a, b -> c, c -> b where
  -- |Create the basic connection structures for a connection between the
  -- current domain and the given one. The returned values should be the
  -- list of grant references to share, a list of ports to share, and a
  -- thunk to invoke when the connection is complete.
  makeConnection    :: DomId -> a -> IO ([GrantRef], [Port], IO c)
  -- |Accept a connection offered by the other side of the rendezvous.
  acceptConnection  :: DomId -> [GrantRef] -> [Port] -> a -> IO b

-- |Given a name for the connection (which should be unique on the host for
-- the duration of the rendezvous) and the special extra information used in
-- the item, create thunks that, when executed, will perform rendezvous
-- between domains.
--
-- Typically, this will be invoked from a shared module, and one domain will
-- use one result while the other will use the other result.
peerConnection :: RendezvousCapable a b c =>
                  String -> a ->
                  (XenStore -> IO b, XenStore -> IO c)
peerConnection name extra = (runLeftSide, runRightSide)
 where
  targetPath = "/rendezvous/" ++ name
  --
  runLeftSide xs = do
    me    <- xsGetDomId xs
    removePath       xs targetPath
    xsMakeDirectory  xs targetPath
    xsSetPermissions xs targetPath [ReadWritePerm me]
    xsWrite          xs (targetPath ++ "/LeftDomId") (show me)
    other  <- read <$> waitForKey xs (targetPath ++ "/RightDomId")
    grants <- read <$> waitForKey xs (targetPath ++ "/RightGrantRefs")
    ports  <- read <$> waitForKey xs (targetPath ++ "/RightPorts")
    res    <- acceptConnection other grants ports extra
    xsWrite xs (targetPath ++ "/LeftConnectionConfirmed") "True"
    return res
  runRightSide xs = do
    other <- read `fmap` waitForKey xs (targetPath ++ "/LeftDomId")
    me    <- xsGetDomId xs
    (gs, ps, confirm) <- makeConnection other extra
    xsWrite xs (targetPath ++ "/RightDomId") (show me)
    xsWrite xs (targetPath ++ "/RightGrantRefs") (show gs)
    xsWrite xs (targetPath ++ "/RightPorts") (show ps)
    _ <- waitForKey xs (targetPath ++ "/LeftConnectionConfirmed")
    removePath xs targetPath
    confirm

clientServerConnection :: RendezvousCapable a b c =>
                          String -> a ->
                          (XenStore -> (b -> IO ()) -> IO (), XenStore -> IO c)
clientServerConnection name extra = (runServer, runClient)
 where
  targetPath = "/rendezvous/" ++ name
  --
  runServer xs callback = do
    me    <- xsGetDomId xs
    removePath xs targetPath
    xsMakeDirectory xs targetPath
    xsWrite         xs (targetPath ++ "/ServerDomId") (show me)
    xsWatch xs targetPath "" $ \ key _ -> do
      case reads (reverse $ takeWhile (/= '/') $ reverse key) of
        [(domid, "")] -> do g <- read <$> waitForKey xs (key ++ "/ClientGrants")
                            p <- read <$> waitForKey xs (key ++ "/ClientPorts")
                            res <- acceptConnection domid g p extra
                            xsWrite xs (key ++ "/ServerConfirmed") "True"
                            callback res -- might as well reuse this thread
        _             -> return ()
  --
  runClient xs = do
    me    <- xsGetDomId xs
    other <- read `fmap` waitForKey xs (targetPath ++ "/ServerDomId")
    (gs, ps, confirm) <- makeConnection other extra
    let targetPath' = targetPath ++ "/" ++ show me
    xsMakeDirectory xs targetPath'
    xsWrite xs (targetPath' ++ "/ClientGrants") (show gs)
    xsWrite xs (targetPath' ++ "/ClientPorts")  (show ps)
    _ <- waitForKey xs (targetPath' ++ "/ServerConfirmed")
    confirm

waitForKey :: XenStore -> String -> IO String
waitForKey xs key = do
  eres <- catch (Right <$> xsRead xs key) leftError
  case eres of
    Left _    -> threadDelay 100000 >> waitForKey xs key
    Right res -> return res
 where
  leftError :: ErrorCode -> IO (Either ErrorCode String)
  leftError = return . Left

removePath :: XenStore -> String -> IO ()
removePath xs str = do catch remSubItems onECContinue
                       catch remItem     onECContinue
 where
  remSubItems = mapM_ (removePath xs) =<< xsDirectory xs str
  remItem     = xsRemove xs str

onECContinue :: ErrorCode -> IO ()
onECContinue _ = return ()
