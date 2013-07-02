-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.OtherDomain(TLBEffect(..),TLBTarget(..)) where

import Hypervisor.Memory(VPtr)
import Hypervisor.Structures.CPUMap

data TLBEffect = NoFlush
               | InvPage TLBTarget
               | FlushTLB TLBTarget

data TLBTarget = AllTLBs
               | MultipleTLBs (VPtr CPUMap)-- ^Should be at least 8-byte aligned
               | LocalTLB

