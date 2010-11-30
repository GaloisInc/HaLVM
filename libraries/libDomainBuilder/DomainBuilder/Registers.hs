{-# LANGUAGE CPP #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
--
module DomainBuilder.Registers (initRegs) where

import DomainBuilder.PageMap

import qualified Hypervisor.Privileged as Priv
import Hypervisor.Memory
import Hypervisor.Basics

import Control.Monad(when)
import Data.Word
import Data.Bits


vCpuNumber :: VCPU
vCpuNumber = VCPU 0 -- FIX - support multiple VCPUs (or not)

flat_kernel_cs :: Word16
flat_kernel_cs = 0xe019

flat_kernel_ds :: Word16
flat_kernel_ds = 0xe021

flat_kernel_ss :: Word16
flat_kernel_ss = flat_kernel_ds

fpuContextSize :: Int
fpuContextSize = 512

initCr3 :: MFN -> Bool -> Bool -> Word32
initCr3 root pageCacheDisable pageWriteThrough
 = let basePcd = if pageCacheDisable
                   then setBit base 4
                   else base
   in if pageWriteThrough
        then setBit basePcd 3
        else basePcd
  where
#ifdef CONFIG_X86_PAE
  -- base  = fromMFN root `shiftL` 7
  base  = fromMFN root `shiftL` 12
#else
  base  = fromMFN root `shiftL` 12
#endif

-- FIX - updateRc passes some more information from the parent to the child
-- other than the contents of r.  This is a hack simply because I
-- (Paul Graunke) do not know what to fill in for some values; thus, the
-- code copies whatever values the parent had.  Perhaps filling in constants
-- would be better in the long run, but in the long run, I would rather not
-- run on Xen anyway.  Try at least setting the eflags to a better default.

-- to create a new register context based (to some extent) on the old one
updateRc :: Integer -- ^ Entry point
         -> MFN     -- ^ Page map
         -> VPage   -- ^ Start info page number
         -> Integer -- ^ Stack
         -> Priv.RegisterContext -> Priv.RegisterContext
updateRc entry pdir (VPage start_info_page) stack rc
 = Priv.RegisterContext {Priv.fpu_ctxt = [0x03, 0x7f] ++
                                        replicate (fpuContextSize - 2) 0
       -- The above sets the FPU state as specified (roughly) by the FINIT
       -- instruction. Essentially it leaves all the registers clear except
       -- the control word, which it initializes to 0x037f.
      ,Priv.gdt_frames = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
      ,Priv.gdt_ents = 0
      ,Priv.ctrlreg = Priv.ControlRegisterSet
          { Priv.cr0 = Priv.cr0 (Priv.ctrlreg rc)
          , Priv.cr1 = 0
          , Priv.cr2 = 0
          , Priv.cr3 = initCr3 pdir False False
          , Priv.cr4 = Priv.cr4 (Priv.ctrlreg rc)
          , Priv.cr5 = 0
          , Priv.cr6 = 0
          , Priv.cr7 = 0
          }
      ,Priv.user_regs
        = Priv.UserRegs
           {Priv.ebx = 0
           ,Priv.ecx = 0
           ,Priv.edx = 4
           ,Priv.esi = fromIntegral start_info_page * pageSize
           ,Priv.edi = 0
           ,Priv.ebp = 0
           ,Priv.eax = 0
           ,Priv.eip = fromIntegral entry
           ,Priv.esp = fromIntegral stack

           ,Priv.eflags = 0x00000200  -- include/asm/processor.h
           ,Priv.error_code = 0
           ,Priv.saved_upcall_mask = 0
           ,Priv.entry_vector = 0

           ,Priv.cs = flat_kernel_cs
           ,Priv.ss = flat_kernel_ss
           ,Priv.es = flat_kernel_ds
           ,Priv.ds = flat_kernel_ds
           ,Priv.fs = flat_kernel_ds
           ,Priv.gs = flat_kernel_ds
           }
      ,Priv.debugreg = Priv.DebugRegisterSet 0 0 0 0 0 0 0 0

      -- ?
      ,Priv.event_callback_cs = 0 -- flat_kernel_cs
      ,Priv.event_callback_eip = 0
      ,Priv.failsafe_callback_cs = 0 -- flat_kernel_cs
      ,Priv.failsafe_callback_eip = 0
      ,Priv.trap_ctxt = []
      ,Priv.vm_assist = 0 -- ?
      ,Priv.cpu_flags = Priv.cpu_flags rc

      ,Priv.ldt_ents = 0
      ,Priv.ldt_base = 0

      ,Priv.kernel_ss = 0
      ,Priv.kernel_sp = 0
      }


-- FIX - see note above about updateRc
initRegs :: DomId   -- ^ Child's domain id
         -> Integer -- ^ Entry point
         -> MFN     -- ^ Machine frame for top-level page map
         -> VPage   -- ^ Virt. page number for the start info page
         -> Integer -- ^ Virt. page for initial stack
         -> IO ()
initRegs dom entry pdir startVAddr stack
 = do when (pdir >= 1024 * 1024)
        $ fail ("CR3 MFN is too large:" ++ show pdir)
      -- writeDebugConsole  ("stack = " ++ show_hex (fromIntegral stack :: Word32) ++ "\n")

      domid_self <- Priv.myDomId
      myDomInfo  <- Priv.domainInfo domid_self
      myRc       <- Priv.domainRegisterContext
                                        (Priv.domain myDomInfo) vCpuNumber
      let new_regs = updateRc entry pdir startVAddr stack myRc
      -- writeDebugConsole  (show new_regs ++ "\n")
      Priv.setDomainRegisterContext dom vCpuNumber new_regs
      



