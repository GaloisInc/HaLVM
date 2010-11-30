-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Control.Concurrent(threadDelay)
import Hypervisor.Basics(SID(..))
import DomainBuilder.Build(buildDomain,domainId)
import DomainBuilder.ModuleInfo(getModule)
import Hypervisor.Debug(writeDebugConsole)
import Hypervisor.Kernel(halvm_kernel)
import Hypervisor.Privileged(unpauseDomain)

main :: IO ()
main = do halvm_kernel [] $ const $ do
          go
          writeDebugConsole "testminios ended now\n"
          threadDelay 5000000

kidSID :: SID
kidSID = SID 3

go :: IO ()
go 
 = do (elf,_) <- getModule 0
      kid <- buildDomain kidSID (5 * 1024) elf "" 
      unpauseDomain (domainId kid)
      return ()
