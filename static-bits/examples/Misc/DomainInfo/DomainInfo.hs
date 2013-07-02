-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.DomainInfo
import Hypervisor.Structures.VCPUContext
import Hypervisor.XenStore

main :: IO ()
main = do
  con <- initXenConsole
  xs  <- initXenStore
  me  <- xsGetDomId xs

  writeConsole con ("Getting domain information for " ++ show me ++ "\n")
  writeDebugConsole ("I am " ++ show me ++ "\n")
  dinfo <- domainInfo me
  writeBit con "Flags" (diFlags dinfo)
  writeBit con "Total pages" (diTotalPages dinfo)
  writeBit con "Max pages" (diMaxPages dinfo)
  writeBit con "Shared pages" (diShrPages dinfo)
  writeBit con "Paged pages" (diPagedPages dinfo)
  writeBit con "CPU Time" (diCPUTime dinfo)
  writeBit con "# of online VCPUS" (diNumOnlineVCPUs dinfo)
  writeBit con "Max VCPU Id" (diMaxVCPUId dinfo)
  writeConsole con "Getting VCPU context:\n"
  vcpuc <- domainProcessorContext me (toVCPU 0)
  writeBit con "VM Assist flags" (rcVMAssist vcpuc)
  writeBit con "Processor flags" (rcProcFlags vcpuc)
  writeConsole con "Completed successfully!\n"

writeBit :: Show a => Console -> String -> a -> IO ()
writeBit con s b = writeConsole con ("  " ++ s ++ ": " ++ show b ++ "\n")
