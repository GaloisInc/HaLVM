-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Control where

import Data.Data

data CreateFlag = CreateHVM | CreateWithHAP | CreateWithS3Integ
                | CreateNoOOSPageTables

instance Eq       CreateFlag
instance Show     CreateFlag
instance Typeable CreateFlag
instance Data     CreateFlag
