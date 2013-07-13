-- BANNERSTARTRight

-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import XenDevice.Console
import Communication.RingBuffer
import XenDevice.Xenbus
import Hypervisor.Kernel
import Hypervisor.Memory
import Hypervisor.Debug
import RendezvousLib.ClientServer(accept)
import ROT13Ring
import ROT13
import Control.Monad
import Hypervisor.Basics

main = halvm_kernel_daemon [dConsole,dXenbus] start
start _ = do
  l <- server rot13convert
  writeDebugConsole "ROT13: Accepting requests.\n"
  forever $ accept l

rot13convert :: BackROT13Ring -> ROT13Request -> IO ROT13Response
rot13convert r (ROT13Request id gref size) =
     do tryGrant <- xTry $ mapGrant (brbDomId r) gref True
        case tryGrant of
          Right (buffer, handle) -> do
                 writeDebugConsole "ROT13: Got request ... "
                 rot13buffer buffer size
                 unmapGrant handle (Just buffer)
                 writeDebugConsole "responding.\n"
                 return $ ROT13Response id 1
          Left _ ->
              return $ ROT13Response id 0

