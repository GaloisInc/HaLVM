-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
{- 
   The purpose of the SafePage module is to wrap unsafe pointer operations
   in safe variations that always perform proper bounds checking.  This
   assumes that I did not mess up this module, so it should be checked
   extra carefully, if anyone actually cared about the quality of a project
   using this module. -}

{- REVISIT - consider adding support for other word sizes (such as Word8). -}
module DomainBuilder.SafePage
  (Page, Offset, getElem, setElem, updateElem, pageBytes, allocPage, freePage
  ,mkPage,unsafe_ptr)
where
import Foreign.Ptr -- evil
import Foreign.Storable -- evil
import Control.Monad(liftM)
import Data.Word(Word8, Word16)
import Hypervisor.Basics(Xen,ignoreErrors)
import qualified Hypervisor.Memory as Mem

type Word12 = Word16 -- REVISIT, consider using a real 12 bit word size
type Offset = Word12 -- REVISIT - consider using a newtype
newtype Page = Page (Ptr Word8) deriving Show

unsafe_ptr :: Page -> Ptr Word8
unsafe_ptr (Page p) = p

pageBytes :: Num a => a
pageBytes = 4096

-- brokenMaybeIO is like fixIO, except you only get to see the type.
-- The function getElem uses it to compute the size of the value being parsed
-- based on the type of the context which calls getElem.
brokenMaybeIO :: (a -> IO (Maybe a)) -> IO (Maybe a)
brokenMaybeIO f = f undefined

getElem :: Storable a => Page -> Offset -> IO (Maybe a)
getElem (Page p) byteOffset
 = brokenMaybeIO (\undef
   -> if byteOffset <= pageBytes - fromIntegral (sizeOf undef)
        then liftM Just (peekByteOff p (fromIntegral byteOffset))
        else return Nothing)

setElem :: Storable a => Page -> Offset -> a -> IO Bool
setElem (Page p) byteOffset x
 = if byteOffset <= pageBytes - fromIntegral (sizeOf x)
     then pokeByteOff p (fromIntegral byteOffset) x >> return True
     else return False

updateElem :: Storable a => Page -> Offset -> (a -> a) -> IO Bool
updateElem (Page p) byteOffset f
 = if byteOffset <= pageBytes - fromIntegral (sizeOf (f undefined))
     then let pind = p `plusPtr` fromIntegral byteOffset
          in peek pind >>= poke pind . f >> return True
     else return False

-- The rest is Xen specific.

-- FIX - create a new monad that carries an IORef or store that
-- is the set of all allocated pages.  On the other hand, maybe we
-- do not free the pages anyway.  On the other hand, this IO (Maybe a)
-- and IO Bool stuff is not the way to do error handling right.  A custom
-- monad would remedy this also.
allocPage :: Xen Page
allocPage = fmap Page Mem.allocPage

freePage :: Page -> IO ()
freePage (Page p) = ignoreErrors $ Mem.freePage p

-- mkPage is not really safe.  The caller must pass it a page not used
-- for other Haskell values.
mkPage :: (Ptr Word8) -> Page
mkPage = Page


