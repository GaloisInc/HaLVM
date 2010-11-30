{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND

-- | A parameterized version of "Data.Generics.Text", in which the
-- read and show functions are parameterized on what function to use
-- for the recursive arguments.  Thus, we can instrument the functions
-- with special behavior on specific types.
--
-- This version also uses "Data.ByteString.Char8" instead of 'String'.

module Data.ByteString.PText ( gshow, gread, pshow, pread, pread', 
                               ReadS, ReadP) where

------------------------------------------------------------------------------

import Control.Monad(mzero)
import Data.Generics.Basics(Data, showConstr, toConstr, gmapQ, 
                            dataTypeOf, fromConstrM, readConstr, Constr)
import Data.Generics.Aliases(extQ, extR)
import Text.ReadP(readP_to_S, readS_to_P, get, char, munch1)
import qualified Text.ReadP as R
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as SBS
import Prelude hiding (ReadS)

------------------------------------------------------------------------------

-- | 'ReadS' specialized for ByteStrings.
type ReadS a = R.ReadS BS.ByteString a
type ReadP a = R.ReadP BS.ByteString Char a

gshow :: Data a => a -> BS.ByteString
gshow = pshow gshow

gread :: Data a => ReadS a
gread = pread gread

-- | Generic, parameterized show: an alternative to \"deriving Show\"
pshow ::  Data a => 
          (forall a'. Data a' => a' -> BS.ByteString) -> a -> BS.ByteString
-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
-- 
pshow f = ( \t ->
             BS.concat ([ BS.pack "("
                        , BS.pack (showConstr (toConstr t))
                        , BS.pack " "
                        ] ++ gmapQ f t 
                          ++ [ BS.pack ")" ])
        ) `extQ` (showByteString . BS.pack :: String -> BS.ByteString)
          `extQ` (showByteString   :: BS.ByteString -> BS.ByteString)
          `extQ` (showSByteString  :: SBS.ByteString -> BS.ByteString)
          `extQ` (BS.singleton     :: Char -> BS.ByteString)

  where
    showByteString s = BS.concat [ BS.pack (show (BS.length s))
                                 , BS.pack " "
                                 , s
                                 ]

    showSByteString s = BS.concat [ SBS.pack (show (SBS.length s))
                                  , SBS.pack " "
                                  , s
                                  ]

-- | Generic, parameterized read: an alternative to \"deriving Read\"
pread :: Data a => (forall a'. Data a' => ReadS a') -> ReadS a
pread f = readP_to_S (pread' (readS_to_P f))

-- | Generic, parameterized read parser.  Likely to be more efficient
-- than 'pread' since it avoids converting between ReadP and ReadS.
pread' :: Data a => (forall a'. Data a' => ReadP a') -> ReadP a
pread' f = allButString `extR` stringCase
                        `extR` byteStringCase
                        `extR` sByteStringCase
                        `extR` charCase

 where

    -- A specific case for strings
    stringCase :: ReadP String
    stringCase = BS.unpack `fmap` byteStringCase

    -- A specific case for bytestrings
    byteStringCase :: ReadP BS.ByteString
    byteStringCase =
      do l <- read `fmap` munch1 (/=' ')
         char ' '
         readS_to_P ((:[]) . BS.splitAt l)

    -- A specific case for strict bytestrings
    sByteStringCase :: ReadP SBS.ByteString
    sByteStringCase =
      do l <- read `fmap` munch1 (/=' ')
         char ' '
         readS_to_P $ \ s -> let (h,t) = BS.splitAt l s in [(h,t)]
    -- A specific case for characters
    charCase :: ReadP Char
    charCase = get

    -- Determine result type
    myDataType = dataTypeOf (getArg allButString)
     where
      getArg :: ReadP a'' -> a''
      getArg = undefined

    -- The generic default for gread
    allButString =
      do char '('
	 str  <- parseConstr		-- Get a lexeme for the constructor
         char ' '
         con  <- str2con str		-- Convert it to a Constr (may fail)
         x    <- fromConstrM f con      -- Read the children
         char ')'
         return x

    -- Turn string into constructor driven by the requested result type,
    -- failing in the monad if it isn't a constructor of this data type
    str2con :: String -> ReadP Constr	
    str2con = maybe mzero return
            . readConstr myDataType

    -- Get a Constr's string at the front of an input string
    parseConstr :: ReadP String
    parseConstr = munch1 (/=' ')
