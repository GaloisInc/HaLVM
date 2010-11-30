{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,ScopedTypeVariables #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import DomainBuilder.Build(buildDomain',setCommandLine
                                       ,domainId,withStartPage)
import DomainBuilder.ModuleInfo(getModule,getModuleByName)
import DomainBuilder.SafePage(setElem,getElem)

import Hypervisor.Basics(SID(..))
import Hypervisor.Debug(writeDebugConsole)
import Hypervisor.Kernel(halvm_kernel)
import Hypervisor.Privileged( unpauseDomain,setIOMemoryPermission
                            , setIOPortPermission,setIRQPermission)

import Control.Concurrent(threadDelay)
import Data.Word
import Data.Bits


main :: IO ()
main = do halvm_kernel [] $ const $ do
          go
          writeDebugConsole "testlinux ended now\n"
          threadDelay 5000000

kidSID :: SID
kidSID = SID 3

go :: IO ()
go
 = do (elf,cmdline) <- getModule 0
      (ramdisk,_)   <- getModuleByName "/boot/ramdisk.cpio.gz"
      (kid,mm) <- buildDomain' kidSID (60 * 1024) elf (Just ramdisk) []
      writeDebugConsole (show mm)
      setCommandLine kid cmdline
      writeDebugConsole "Linux domain built\n"

      withStartPage kid $ \ page -> do
        Just (curFlags::Word32) <- getElem page 40
        setElem page 40 $ curFlags .|. 3
      -- To generate these magic numbers:
      --  printf("offset = %i, orval = %i\n",
      --         __builtin_offsetof(struct start_info, flags),
      --         SIF_INITDOMAIN | SIF_PRIVILEGED);
      setIOMemoryPermission (domainId kid) 0 0xFFFFF True
      setIOPortPermission (domainId kid) 0 0xFFFF True
      mapM_ (flip (setIRQPermission (domainId kid)) True) [0..255]

      unpauseDomain (domainId kid)
      return ()
