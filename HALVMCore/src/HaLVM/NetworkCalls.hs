module HaLVM.NetworkCalls(
         setNetworkStackImplementation
       )
 where

import Control.Monad
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import HaLVM.NetworkCalls.TH


data NetworkStackImplementation = NetworkStackImplementation {
       accept4     :: Accept4
     , bind        :: Bind
     , connect     :: Connect
     , getPeerName :: GetPeerName
     , getSockName :: GetSockName
     , getSockOpt  :: GetSockOpt
     , setSockOpt  :: SetSockOpt
     , listen      :: Listen
     , recvFrom    :: RecvFrom
     , sendTo      :: SendTo
     , recvMsg     :: RecvMsg
     , sendMsg     :: SendMsg
     , shutdown    :: Shutdown
     , socket      :: Socket
     , socketPair  :: SocketPair
     }

type NetworkImplementationState = StablePtr NetworkStackImplementation

setNetworkStackImplementation :: NetworkStackImplementation -> IO ()
setNetworkStackImplementation nsi =
  do nsiSP <- newStablePtr nsi
     alloca $ \ oldptr ->
       do haveOld <- setCurrentImplementation nsiSP oldptr
          when haveOld $
            do old <- peek oldptr
               freeStablePtr old

foreign import ccall unsafe
  setCurrentImplementation :: StablePtr NetworkStackImplementation ->
                              Ptr (StablePtr NetworkStackImplementation) ->
                              IO Bool

-- -----------------------------------------------------------------------------

type SockAddr = ()
type MsgHdr   = ()
type SockLen  = Word32

-- -----------------------------------------------------------------------------

type Accept4 = NetworkImplementationState ->
               CInt -> Ptr SockAddr -> Ptr SockLen -> CInt -> Ptr CInt ->
               IO CInt

haskell_network_accept4 :: Accept4
haskell_network_accept4 = undefined

foreign export ccall haskell_network_accept4 :: Accept4

-- -----------------------------------------------------------------------------

type Bind = NetworkImplementationState ->
            CInt -> Ptr SockAddr -> SockLen ->
            IO CInt

haskell_network_bind :: Bind
haskell_network_bind = undefined

foreign export ccall haskell_network_bind :: Bind

-- -----------------------------------------------------------------------------

type Connect = NetworkImplementationState ->
               CInt -> Ptr SockAddr -> SockLen ->
               IO CInt

haskell_network_connect :: Connect
haskell_network_connect = undefined

foreign export ccall haskell_network_connect :: Connect

-- -----------------------------------------------------------------------------

type GetPeerName = NetworkImplementationState ->
                   CInt -> Ptr SockAddr -> Ptr SockLen ->
                   IO CInt

haskell_network_getpeername :: GetPeerName
haskell_network_getpeername = undefined

foreign export ccall haskell_network_getpeername :: GetPeerName

-- -----------------------------------------------------------------------------

type GetSockName = NetworkImplementationState ->
                   CInt -> Ptr SockAddr -> Ptr SockLen ->
                   IO CInt

haskell_network_getsockname :: GetSockName
haskell_network_getsockname = undefined

foreign export ccall haskell_network_getsockname :: GetSockName

-- -----------------------------------------------------------------------------

type GetSockOpt = NetworkImplementationState ->
                  CInt -> CInt -> CInt -> Ptr () -> Ptr SockLen ->
                  IO CInt

haskell_network_getsockopt :: GetSockOpt
haskell_network_getsockopt = undefined

foreign export ccall haskell_network_getsockopt :: GetSockOpt

-- -----------------------------------------------------------------------------

type SetSockOpt = NetworkImplementationState ->
                  CInt -> CInt -> CInt -> Ptr () -> SockLen ->
                  IO CInt

haskell_network_setsockopt :: SetSockOpt
haskell_network_setsockopt = undefined

foreign export ccall haskell_network_setsockopt :: SetSockOpt

-- -----------------------------------------------------------------------------

type Listen = NetworkImplementationState ->
              CInt -> CInt ->
              IO CInt

haskell_network_listen :: Listen
haskell_network_listen = undefined

foreign export ccall haskell_network_listen :: Listen

-- -----------------------------------------------------------------------------

type RecvFrom = NetworkImplementationState ->
                CInt -> Ptr () -> CSize -> CInt ->
                Ptr SockAddr -> Ptr SockLen ->
                IO CSize

haskell_network_recvfrom :: RecvFrom
haskell_network_recvfrom = undefined

foreign export ccall haskell_network_recvfrom :: RecvFrom

-- -----------------------------------------------------------------------------

type RecvMsg = NetworkImplementationState ->
               CInt -> Ptr MsgHdr -> CInt ->
               IO CSize

haskell_network_recvmsg :: RecvMsg
haskell_network_recvmsg =  undefined

foreign export ccall haskell_network_recvmsg :: RecvMsg

-- -----------------------------------------------------------------------------

type SendMsg = NetworkImplementationState ->
               CInt -> Ptr MsgHdr -> CInt ->
               IO CSize

haskell_network_sendmsg :: SendMsg
haskell_network_sendmsg =  undefined

foreign export ccall haskell_network_sendmsg :: SendMsg

-- -----------------------------------------------------------------------------

type SendTo = NetworkImplementationState ->
              CInt -> Ptr () -> CSize -> CInt ->
              Ptr SockAddr -> SockLen ->
              IO CSize

haskell_network_sendto :: SendTo
haskell_network_sendto =  undefined

foreign export ccall haskell_network_sendto :: SendTo

-- -----------------------------------------------------------------------------

type Shutdown = NetworkImplementationState ->
                CInt -> CInt ->
                IO CInt

haskell_network_shutdown :: Shutdown
haskell_network_shutdown =  undefined

foreign export ccall haskell_network_shutdown :: Shutdown

-- -----------------------------------------------------------------------------

type Socket = NetworkImplementationState ->
              CInt -> CInt -> CInt ->
              IO CInt

haskell_network_socket :: Socket
haskell_network_socket = undefined

foreign export ccall haskell_network_socket :: Socket

-- -----------------------------------------------------------------------------

type SocketPair = NetworkImplementationState ->
                  CInt -> CInt -> CInt -> Ptr CInt ->
                  IO CInt

haskell_network_socketpair :: SocketPair
haskell_network_socketpair = undefined

foreign export ccall haskell_network_socketpair :: SocketPair


