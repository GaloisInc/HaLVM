-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- Blank for Haddock
{- |Constants for special virtual interrupts originating in the hypervisor.

VIRTUAL INTERRUPTS

Virtual interrupts that a guest OS may receive from Xen.

In the side comments, V. denotes a per-VCPU VIRQ while G. denotes a
global VIRQ. The former can be bound once per VCPU and cannot be re-bound.
The latter can be allocated only once per guest: they must initially be
allocated to VCPU0 but can subsequently be re-bound.

-}

module Hypervisor.Virqs where

import Data.Word(Word32)

#include <stdint.h>
#include <xen/xen.h>

-- | V. Timebase update, and\/or requested timeout.
virqTIMER :: Word32
virqTIMER = (#const VIRQ_TIMER)

-- | V. Request guest to dump debug info.
virqDEBUG :: Word32
virqDEBUG = (#const VIRQ_DEBUG)

-- | G. (DOM0) Bytes received on emergency console.
virqCONSOLE :: Word32
virqCONSOLE = (#const VIRQ_CONSOLE)

-- | G. (DOM0) Exceptional event for some domain.
virqDOM_EXC :: Word32
virqDOM_EXC = (#const VIRQ_DOM_EXC)

-- | G. (DOM0) A domain has paused for debugging.
virqDEBUGGER :: Word32
virqDEBUGGER = (#const VIRQ_DEBUGGER)

#ifdef VIRQ_TBUF
-- | G. (DOM0) Trace buffer has records available.
virqTBUF :: Word32
virqTBUF = (#const VIRQ_TBUF)
#endif

#ifdef VIRQ_XENOPROF
-- | V. XenOprofile interrupt: new sample available
virqXENOPROF :: Word32
virqXENOPROF = (#const VIRQ_XENOPROF)
#endif

#ifdef VIRQ_CON_RING
-- | G. (DOM0) Bytes received on the console
virqCON_RING :: Word32
virqCON_RING  = (#const VIRQ_CON_RING)
#endif

#ifdef VIRQ_PCPU_STATE
-- | G. (DOM0) PCPU state chagned
virqPCPU_STATE :: Word32
virqPCPU_STATE  = (#const VIRQ_PCPU_STATE)
#endif

#ifdef VIRQ_MEM_EVENT
-- | G. (DOM0) A memory event has occured
virqMEM_EVENT :: Word32
virqMEM_EVENT  = (#const VIRQ_MEM_EVENT)
#endif

#ifdef VIRQ_XC_RESERVED
-- | G. Reserved for XenClient
virqXC_RESERVED :: Word32
virqXC_RESERVED  = (#const VIRQ_XC_RESERVED)
#endif

#ifdef VIRQ_ENOMEM
-- | G. (DOM0) Low on heap memory
virq_ENOMEM :: Word32
virq_ENOMEM  = (#const VIRQ_ENOMEM)
#endif

#ifdef VIRQ_ARCH_0
-- | (unknown) Architecture-specific VIRQ
virq_ARCH0 :: Word32
virq_ARCH0  = (#const VIRQ_ARCH_0)
#endif

#ifdef VIRQ_ARCH_1
-- | (unknown) Architecture-specific VIRQ
virq_ARCH1 :: Word32
virq_ARCH1  = (#const VIRQ_ARCH_1)
#endif

#ifdef VIRQ_ARCH_2
-- | (unknown) Architecture-specific VIRQ
virq_ARCH2 :: Word32
virq_ARCH2  = (#const VIRQ_ARCH_2)
#endif

#ifdef VIRQ_ARCH_3
-- | (unknown) Architecture-specific VIRQ
virq_ARCH3 :: Word32
virq_ARCH3  = (#const VIRQ_ARCH_3)
#endif

#ifdef VIRQ_ARCH_4
-- | (unknown) Architecture-specific VIRQ
virq_ARCH4 :: Word32
virq_ARCH4  = (#const VIRQ_ARCH_4)
#endif

#ifdef VIRQ_ARCH_5
-- | (unknown) Architecture-specific VIRQ
virq_ARCH5 :: Word32
virq_ARCH5  = (#const VIRQ_ARCH_5)
#endif

#ifdef VIRQ_ARCH_6
-- | (unknown) Architecture-specific VIRQ
virq_ARCH6 :: Word32
virq_ARCH6  = (#const VIRQ_ARCH_6)
#endif

#ifdef VIRQ_ARCH_7
-- | (unknown) Architecture-specific VIRQ
virq_ARCH7 :: Word32
virq_ARCH7  = (#const VIRQ_ARCH_7)
#endif

