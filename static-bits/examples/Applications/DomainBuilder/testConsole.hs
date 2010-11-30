-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Control.Concurrent(threadDelay)
import DomainBuilder.Build(buildDomain', setConsoleDomUMfn, setConsoleDomUEvtChn, domainId, MemMap(..))
-- import DomainBuilder.CreateVM(domainId,allocBlankChildPageMfn)
import Hypervisor.Port(allocUnboundPort)
import DomainBuilder.ModuleInfo(getModule)
import Hypervisor.Debug(writeDebugConsole)
import Hypervisor.Kernel(halvm_kernel)
import Hypervisor.Privileged(unpauseDomain)
import DomainBuilder.IfaceTypes(Access(Access_readWrite))
import Hypervisor.Basics(SID(..),domidSelf)
-- import Hypervisor.Memory(toMFN)

import XenDevice.ConsoleBackend(attachConsole, send, recv)

main :: IO ()
main = do halvm_kernel [] $ const $ do 
          go
          writeDebugConsole "testConsole ended now\n"
          threadDelay 5000000

kidSID :: SID
kidSID = SID 3

go :: IO ()
go 
 = do (elf,_) <- getModule 0
      (kid,mem_map) <-
         buildDomain' kidSID (64 * 1024) elf Nothing [Access_readWrite]
         -- last arg was: "allocBlankChildPageMfn Access_readWrite Nothing"
         -- "[Access_readWrite]" cribbed from testIvc.hs
      let (_,[mfn]) = blank_pages mem_map
        -- Was: let mfn = toMFN (fromIntegral mfn')
        -- The above alternative also cribbed from testIvc.hs
      Right port <- allocUnboundPort (domainId kid) domidSelf
      setConsoleDomUMfn kid mfn
      setConsoleDomUEvtChn kid port
      ch <- attachConsole (domainId kid,mfn,port)
      unpauseDomain (domainId kid)

      send ch "Hello kiddo\n"
      echo "Got back: " =<< getLine (recv ch)
      echo "Got back: " =<< getLine (recv ch)
      
      send ch "Hello kiddo again\n"
      echo "Got back again: " =<< getLine (recv ch)
      echo "Got back again: " =<< getLine (recv ch)

echo :: String -> String -> IO ()
echo pre s = writeDebugConsole $ pre ++ show s ++ "\n"

getLine :: (Int -> IO String) -> IO String
getLine readch = getln ""
  where getln l = 
          do [c] <- readch 1
             case c of
               '\n' -> return (reverse l)
               '\r' -> getln l
               _    -> getln (c:l)
