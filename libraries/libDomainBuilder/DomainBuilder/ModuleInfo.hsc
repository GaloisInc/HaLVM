-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
-- empty for Haddock
-- | Support for fetching modules passed in from Xen at boot.

module DomainBuilder.ModuleInfo(ModuleInfo(..), getModuleCount,
getModule, getModuleByName, isMultiModulesBoot, moduleImageString,
moduleInfo, moduleNames, moduleName) where

import Control.Monad(when)
import Data.Word(Word32)
import Foreign.C.String(peekCString)
import Foreign.Ptr(wordPtrToPtr)
import Foreign.Storable(Storable(..))
import Hypervisor.StartInfo(getStartInfo, getModStart, getModLen)
import System.Environment(getArgs)
import qualified Data.ByteString.Char8 as BS

-- |One record in the list of modules passed in by Grub.
data ModuleInfo = ModuleInfo 
  { mod_start :: Word32 -- ^The starting address of the module.
  , mod_end   :: Word32 -- ^The ending address of the module.
  , string    :: String -- ^The line in the grub configuration file (menu.lst
                        -- or grub.conf, usually) describing this module.
  } deriving (Eq,Show,Read)

wordSize :: Int
wordSize = sizeOf (undefined :: Word32)

instance Storable ModuleInfo where
  sizeOf _    = 3*wordSize
  alignment _ = 4
  peek p      = 
    do ms <- peekByteOff p 0
       me <- peekByteOff p wordSize
       sa <- peekByteOff p (2*wordSize)
       s <- peekCString sa
       return ModuleInfo { mod_start = ms, mod_end = me, string = s }

-- | Return the number of modules passed in at boot.
getModuleCount :: IO Int
getModuleCount =
  do mm <- isMultiModulesBoot
     si <- getStartInfo
     ml <- getModLen si
     return (if mm then (fromIntegral ml) else if ml > 0 then 1 else 0)

-- | Return the i:th module and its parameter string, counting from
-- zero.  Works even if the system wasn't booted with the multimodules
-- option.
getModule :: Int -> IO (BS.ByteString,String)
getModule i =
  do mm <- isMultiModulesBoot
     si <- getStartInfo
     ms <- getModStart si
     ml <- getModLen si
     if mm then
       do when (i < 0 || i >= fromIntegral ml) $ 
            fail $ "getModule: index "++show i++" is out of range."
          mi <- peekElemOff (wordPtrToPtr (fromIntegral ms)) i
          mis <- moduleImageString mi
          return mis
       else
       do when (i > 0 || ms == 0) $ 
            fail $ "getModule: index "++show i++" is out of range."
          bstr <- BS.packCStringLen ( wordPtrToPtr (fromIntegral ms)
                                    , fromIntegral ml)
          return ( bstr , "")

-- | Find a module by name, where the name of a module is given by
-- 'moduleName'.
getModuleByName :: String -> IO (BS.ByteString,String)
getModuleByName s = 
  do mi <- moduleInfo
     case lookup s [(moduleName m, m) | m <- mi] of
       Just m -> moduleImageString m
       Nothing -> fail $ "getModuleByName: no such module: "++show s

-- | Return true if Xen was booted with the ''multimodules'' option.
isMultiModulesBoot :: IO Bool
isMultiModulesBoot = (("multimodules" `elem`) . words . head) `fmap` getArgs

-- | Fetch a loadable image and a parameter string given a ModuleInfo.
moduleImageString :: ModuleInfo -> IO (BS.ByteString, String)
moduleImageString mi = do
  bstr <- BS.packCStringLen ( wordPtrToPtr (fromIntegral (mod_start mi))
                            , fromIntegral ((mod_end mi - mod_start mi)))
  return (bstr, string mi)

-- | Return a list of 'ModuleInfo' for all modules passed in at boot.
moduleInfo :: IO [ModuleInfo]
moduleInfo =
  do mm <- isMultiModulesBoot
     when (not mm) $ 
       fail "moduleInfo: Xen not booted with multimodules option."
     si <- getStartInfo
     ml <- getModLen si
     ms <- getModStart si
     sequence $ map (peekElemOff (wordPtrToPtr (fromIntegral ms)))
                    [0..fromIntegral ml-1]

-- | Return a list of names of all modules passed at boot.
moduleNames :: IO [String]
moduleNames = map moduleName `fmap` moduleInfo

-- | Return the name of a module.  The name is the first word of the
-- parameter string, which is typically the pathname argument to the
-- module command in Grub.
moduleName :: ModuleInfo -> String
moduleName mi = case words (string mi) of []  -> ""
                                          n:_ -> n

