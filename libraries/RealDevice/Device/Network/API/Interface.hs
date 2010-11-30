-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.Network.API.Interface where


data Interface m i o
  = Interface { rx :: m i, tx :: o -> m () }

data TimedInterface m i o
  = TimedInterface { rxT :: Maybe Int -> m (Maybe i), txT :: o -> m () }
