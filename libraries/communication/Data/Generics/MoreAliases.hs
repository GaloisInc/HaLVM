{-# LANGUAGE Rank2Types #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND

-- | This adds some 'Typeable2' generics combinators, 
-- which ought to be in Data.Generics.
module Data.Generics.MoreAliases ( ext2Q, ext2R ) where

import Data.Generics(Data, dataCast2, Typeable2)

-- | Flexible type extension
ext2 :: (Data a, Typeable2 t)
     => c a
     -> (forall d b. (Data d, Data b) => c (t d b))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)

-- | Type extension of queries for type constructors
ext2Q :: (Data d, Typeable2 t)
      => (d -> q)
      -> (forall f e. (Data f, Data e) => t f e -> q)
      -> d -> q
ext2Q def ext = unQ ((Q def) `ext2` (Q ext))


-- | Type extension of readers for type constructors
ext2R :: (Monad m, Data d, Typeable2 t)
      => m d
      -> (forall g e. (Data g, Data e) => m (t g e))
      -> m d
ext2R def ext = unR ((R def) `ext2` (R ext))

------------------------------------------------------------------------------
--
--	Type constructors for type-level lambdas
--
------------------------------------------------------------------------------

-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | The type constructor for readers
newtype R m x = R { unR :: m x }


