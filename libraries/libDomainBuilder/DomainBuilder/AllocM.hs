-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
--
module DomainBuilder.AllocM
  ( AllocM
  , run_alloc
  , alloc_local
  , alloc_page
  , alloc_zero
  , alloc_zeros
  , alloc_string

  , vpage_range
  , last_vpage
  , mfn_list
  , mfn_num
  , lookup_mfn
  , io
  ) where

import Hypervisor.Basics(DomId)
import Hypervisor.Memory(MFN,pageSize)
import DomainBuilder.PageMap as PageMap
import DomainBuilder.IfaceTypes (Access(..))
import DomainBuilder.SafePage1

import Data.Array
import Data.Word
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Control.Monad(when)
import Foreign.Storable


-- | Read-only data used during the construction process.
data RO = RO
  { childId :: DomId
    -- ^ The id of the domain that is being constructed.

  , mfns :: Array VPage MFN
    -- ^ The pool of available machine frames---we should use only machine
    -- frames that are in this array.
    -- This also doubles as the physical-to-machine map, except that
    -- is starts at the virtual base address, instead of 0.
  }


-- | Mutable data used during the construction process.
data RW = RW
  { pagemap :: Map.Map VPage Access
    -- ^ The page map for the child domain.
    -- Keeps tracks of which address are mapped, and their permissions.
    -- We do not keep track of MFNs because we use a 1-1 mapping.
  }


-- | The allocation monad.
newtype AllocM a = A { unA :: RO -> RW -> IO (a,RW) }


-- | Execute an IO computation in the context of the 'AllocM' monad.
io :: IO a -> AllocM a
io m = A $ \_ s -> do a <- m
                      return (a,s)


-- | Allocate an unitialized machine frame for the child.
-- The 'access' parameter specifies the permissions for the mapping.
-- The page is not initialized, but it is mapped into our address
-- space so that we can initialize it later.
-- NOTE 1: We should remember to unmap the page when we are done with it.
-- NOTE 2: The virtual addresses passed this function should be in
-- the range (vbase...vbase+max mem size-1), where vbase is the
-- virtual base extracted from the Xen notes.
alloc_local :: Storable a => VPage -> Access -> AllocM (Page a)
alloc_local vaddr access =
  do mfn <- lookup_mfn vaddr
     A $ \r s ->
       do a <- map_page (childId r) mfn
          let m1  = Map.insert vaddr access (pagemap s)
          return (a,s { pagemap = m1 })


-- | Allocate a machine frame for the given virtual page number.
-- The 'access' parameter specifies the permissions for the mapping.
-- The last argument is a function that should initialize the page.
-- NOTE 1: The parameter of the initialization function is valid only
-- until the init. function returns.
-- NOTE 2: The virtual addresses passed this function should be in
-- the range (vbase...vbase+max mem size-1), where vbase is the
-- virtual base extracted from the Xen notes.
alloc_page :: Storable a => VPage -> Access -> (Page a -> IO b) -> AllocM b
alloc_page vaddr access f =
  do page <- alloc_local vaddr access
     io $ do b <- f page
             unmap_page page
             return b

-- | Find the smalles and largest virtual addresses that are
-- currently mapped in the linear address space.
-- WARNING: This will crash if the page map is empty.
vpage_range :: AllocM (VPage,VPage)
vpage_range = A $ \_ s -> let first_addr = fst (Map.findMin (pagemap s))
                              last_addr  = fst (Map.findMax (pagemap s))
                          in return ((first_addr,last_addr),s)


-- | Returns the available MFNs.  The MFNs are in the order that they
-- should be put in the physical to machine map.
mfn_list :: AllocM [MFN]
mfn_list = A $ \r s -> return (elems (mfns r), s)

-- | How many machine frames do we have?
mfn_num  :: AllocM Word
mfn_num = A $ \r s -> let (VPage x,VPage y) = bounds (mfns r)
                      in return (fromIntegral (y-x+1),s)

-- | Get the machine frame number that would be\/is associated with
-- the given virtual page number.  Because we use a 1-1 mapping,
-- we can just lookup the virtual page number in the array of MFNs.
lookup_mfn :: VPage -> AllocM MFN
lookup_mfn p = A $ \r s -> do let b = bounds (mfns r)
                              when (not (inRange b p))
                                $ fail $ "Page " ++ show p ++
                                        " is not in the range " ++ show b
                              return (mfns r ! p,s)



-- | Execute a computaion in the allocation monad.
-- After we are finished with allocation, we also store the page map
-- structures at the end of the address space (breadth first order).
-- We also add some pages to pad to a 4MB boundary. (see DomainBuilder.PageMap)
-- If everything works out, then we return:
--   * the result of the computation
--   * the virtual page number of the first page map strucutre
--   * the machine frame number for the first page map structure
--   * the number of pages used for page maps
run_alloc :: DomId    -- ^ The id of the child under construction.
          -> VPage    -- ^ The virtual base address for the child.
          -> [MFN]    -- ^ A list of available machine frames.
          -> AllocM a -- ^ Computation that allocates the kernel data.
          -> IO (a,VPage,MFN,Word)
run_alloc dom vbase ms (A f) =
  do (a,s) <- f ro rw
     let m = pagemap s
     (vaddr,size) <- PageMap.write_pmaps dom vbase (mfns ro) m
     let mfn = mfns ro ! vaddr -- Machine frame for top page
     return (a,vaddr,mfn,size)
  where ro = RO { childId = dom
                , mfns    = array (vbase,maxbd) (zip [vbase..] ms)
                }
        rw = RW { pagemap = Map.empty }
        maxbd = vbase + fromIntegral (length ms) - 1




--------------------------------------------------------------------------------

instance Functor AllocM where
  fmap f m = do a <- m
                return (f a)

instance Monad AllocM where
  return a  = A (\_ s -> return (a,s))
  A m >>= f = A (\r s -> do (a,s1) <- m r s
                            unA (f a) r s1)


-- derived
--------------------------------------------------------------------------------

-- | Allocate a page full of 0.
alloc_zero :: VPage -> Access -> AllocM ()
alloc_zero vaddr access = alloc_page vaddr access zero_page

-- | Allocate a sequnce of pages filled with 0.
alloc_zeros :: VPage -> Access -> Word -> AllocM ()
alloc_zeros vaddr access num =
  mapM_ (`alloc_zero` access) $ take (fromIntegral num) [vaddr .. ]


-- | Allocate a sequence of adject virtual pages, which are initialized
-- from the given bytestring.  The last page is padded with 0, if the
-- numer of bytes is not a multiple of 'pageSize'
alloc_string :: VPage -> Access -> BS.ByteString -> AllocM ()
alloc_string vaddr access s = loop vaddr s
  where loop a p
          | bytes == 0        = return ()
          | bytes >= pg       = do alloc_page a access (copy_bs p)
                                   loop (a+1) (BS.drop pg p)
          | otherwise         = alloc_page a access $ \page ->
                                  do zero_page page
                                     copy_bs p page
          where bytes = BS.length p
                pg    = fromIntegral pageSize

last_vpage :: AllocM VPage
last_vpage = snd `fmap` vpage_range


