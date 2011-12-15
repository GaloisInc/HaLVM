{-# LANGUAGE Rank2Types #-}

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

module Data.PText ( pshow, pread, pread') where

------------------------------------------------------------------------------

import Control.Monad(mzero)
import Data.Generics.Basics(Data, showConstr, toConstr, gmapQ, 
                            dataTypeOf, fromConstrM, readConstr, Constr)
import Data.Generics.Aliases(extQ, extR)
import Text.ParserCombinators.ReadP(readP_to_S, readS_to_P, ReadP,
                                   skipSpaces, char, string, (<++), munch1)

------------------------------------------------------------------------------


-- | Generic, parameterized show: an alternative to \"deriving Show\"
pshow ::  Data a => (forall a'. Data a' => a' -> String) -> a -> String
-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
-- 
pshow f = ( \t ->
                "("
             ++ showConstr (toConstr t)
             ++ concat (gmapQ ((++) " " . f) t)
             ++ ")"
        ) `extQ` (show :: String -> String)
          `extQ` (show :: Char -> String)

-- | Generic, parameterized read: an alternative to \"deriving Read\"
pread :: Data a => (forall a'. Data a' => ReadS a') -> ReadS a
pread f = readP_to_S (pread' (readS_to_P f))

-- | Generic, parameterized read parser.  Likely to be more efficient
-- than 'pread' since it avoids converting between ReadP and ReadS.
pread' :: Data a => (forall a'. Data a' => ReadP a') -> ReadP a
pread' f = allButString `extR` stringCase
                        `extR` charCase

 where

    -- A specific case for strings
    stringCase :: ReadP String
    stringCase = readS_to_P reads

    -- A specific case for characters
    charCase :: ReadP Char
    charCase = readS_to_P reads

    -- Determine result type
    myDataType = dataTypeOf (getArg allButString)
     where
      getArg :: ReadP a'' -> a''
      getArg = undefined

    -- The generic default for gread
    allButString =
      do
 		-- Drop "  (  "
         skipSpaces			-- Discard leading space
         _ <- char '('			-- Parse '('
         skipSpaces			-- Discard following space

		-- Do the real work
	 str  <- parseConstr		-- Get a lexeme for the constructor
         con  <- str2con str		-- Convert it to a Constr (may fail)
         x    <- fromConstrM f con      -- Read the children

		-- Drop "  )  "
         skipSpaces			-- Discard leading space
         _ <- char ')'			-- Parse ')'
         skipSpaces			-- Discard following space

         return x

    -- Turn string into constructor driven by the requested result type,
    -- failing in the monad if it isn't a constructor of this data type
    str2con :: String -> ReadP Constr	
    str2con = maybe mzero return
            . readConstr myDataType

    -- Get a Constr's string at the front of an input string
    parseConstr :: ReadP String
    parseConstr =  
               string "[]"     -- Compound lexeme "[]"
          <++  infixOp	       -- Infix operator in parantheses
          <++  readS_to_P lex  -- Ordinary constructors and literals

    -- Handle infix operators such as (:)
    infixOp :: ReadP String
    infixOp = do c1  <- char '('
                 str <- munch1 (not . (==) ')')
	         c2  <- char ')'
                 return $ [c1] ++ str ++ [c2]
