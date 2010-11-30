
--------------------------------------------------------------------------------
-- |
-- Module      : Testing.Strings
-- Copyright   : (c) 2008-2009 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : adams-moran@galois.com
-- Stability   :
-- Portability : non-portable (concurrency)
--

module Testing.Strings ( -- * Functions
                         randomStrings -- :: Orc [String]
                       )
where

import Orc.Monad

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random
import Data.Char


randomStrings :: Orc [String]
randomStrings = ioOrc $ do
  gen <- newStdGen
  let size = 15
  return $ unGen (listOf1 $ listOf1 $ suchThat arbitrary validChar) gen size
  where
    validChar :: Char -> Bool
    validChar ch = isAscii ch && isPrint ch && isNotRegexSpecial ch

    isNotRegexSpecial :: Char -> Bool
    isNotRegexSpecial = not . (`elem` regexSpecialChars)

    regexSpecialChars = [ '[', ']'
                        , '(', ')'
                        , '{', '}'
                        , '+', '*', '?'
                        , '^', '$'
                        , '\\', '!'
                        , '.'
                        ]

