-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- | Console backend operations.
module XenDevice.ConsoleBackend(ConsoleAddress, ConsoleHandle, mkConsoleTo, open, send, recv, close, attachConsole)
 where

import qualified XenDevice.ConsoleInterface as XI
import Hypervisor.Basics(DomId,ignoreErrors)
import Hypervisor.Port(Port,allocPort,bindRemotePort,closePort)
import Hypervisor.EventWaitSet(EventWaitSet)
import Hypervisor.Memory(allocPage,mapForeignMachineFrames,MFN,VPtr,vptrToMFN)
import Util.WaitSet(wait)
import Foreign.Ptr(Ptr,plusPtr,castPtr)
import Foreign.C.Types(CChar)
import Foreign.C.String(withCStringLen,peekCStringLen)
import Foreign.Marshal.Array(allocaArray)
import Control.Monad(when)


type ConsoleAddress = (DomId, MFN, Port)

data ConsoleHandle = CH{ waitset :: EventWaitSet
                      , canWrite :: IO Bool
                      , canRead :: IO Bool
                      , coWrite :: Ptr CChar -> Int -> IO Int
                      , coRead :: Ptr CChar -> Int -> IO Int
                      , lport :: Port
                      , page :: VPtr CChar
                      }

mkConsoleTo :: DomId -> IO (VPtr a, ConsoleAddress)
mkConsoleTo dom =
  do v <- allocPage 
     mfn <- vptrToMFN v
     port <- allocPort dom
     return (v, (dom, mfn, port))

attachConsole :: ConsoleAddress -> IO ConsoleHandle
attachConsole (dom, mfn, rport) =
  do pg <- mapForeignMachineFrames dom [mfn]
     port <- bindRemotePort dom rport
     open pg port

open :: VPtr a -> Port -> IO ConsoleHandle
open pg port = 
  do i <- XI.interface pg port 
     return CH{ waitset = XI.waitSet i
              , canWrite = XI.canWriteConsole (XI.in_prodp i) (XI.in_consp i) 
                             (XI.in_ring_size i)
              , canRead = XI.canReadConsole (XI.out_prodp i) (XI.out_consp i)
              , coWrite = XI.writeConsole (XI.in_prodp i) (XI.in_consp i) 
                            (XI.in_shdata i) port (XI.in_ring_size i)
              , coRead = XI.readConsole (XI.out_prodp i) (XI.out_consp i)
                           (XI.out_shdata i) port (XI.out_ring_size i)
              , lport = port
              , page = castPtr pg
              }

send :: ConsoleHandle -> String -> IO ()
send h s = withCStringLen s send'
  where send' (a,l) = do wait (waitset h) (canWrite h)
                         len <- coWrite h a l
                         when (len < l) $ send' (a `plusPtr` len,l-len)

recv :: ConsoleHandle -> Int -> IO String
recv h l = 
  do wait (waitset h) (canRead h)
     s <- allocaArray l
       (\a -> do len <- coRead h a l
                 peekCStringLen (a,len))
     if null s then recv h l else return s

close :: ConsoleHandle -> IO ()
close h = ignoreErrors $ closePort (lport h)
