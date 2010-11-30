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
