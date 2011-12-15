{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ReadP
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (local universal quantification)
--
-- This is a library of parser combinators, originally written by Koen Claessen.
-- It parses all alternatives in parallel, so it never keeps hold of 
-- the beginning of the input string, a common source of space leaks with
-- other parsers.  The '(+++)' choice combinator is genuinely commutative;
-- it makes no difference which branch is "shorter".
--
-- This is derived from "Text.ParserCombinators.ReadP".  
-- Parameterized over the string type so that we can parse e.g. 
-- bytestrings too.

-----------------------------------------------------------------------------

module Text.ReadP
  ( 
  -- * The 'ReadP' type
  ReadP,      -- :: * -> *; instance Functor, Monad, MonadPlus
  
  -- * Primitive operations
  get,        -- :: ReadP Char
  look,       -- :: ReadP String
  (+++),      -- :: ReadP a -> ReadP a -> ReadP a
  (<++),      -- :: ReadP a -> ReadP a -> ReadP a
  gather,     -- :: ReadP a -> ReadP (String, a)
  
  -- * Other operations
  pfail,      -- :: ReadP a
  satisfy,    -- :: (Char -> Bool) -> ReadP Char
  char,       -- :: Char -> ReadP Char
  string,     -- :: String -> ReadP String
  munch,      -- :: (Char -> Bool) -> ReadP String
  munch1,     -- :: (Char -> Bool) -> ReadP String
  skipSpaces, -- :: ReadP ()
  choice,     -- :: [ReadP a] -> ReadP a
  count,      -- :: Int -> ReadP a -> ReadP [a]
  between,    -- :: ReadP open -> ReadP close -> ReadP a -> ReadP a
  option,     -- :: a -> ReadP a -> ReadP a
  optional,   -- :: ReadP a -> ReadP ()
  many,       -- :: ReadP a -> ReadP [a]
  many1,      -- :: ReadP a -> ReadP [a]
  skipMany,   -- :: ReadP a -> ReadP ()
  skipMany1,  -- :: ReadP a -> ReadP ()
  sepBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  sepBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  chainr,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  chainr1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  manyTill,   -- :: ReadP a -> ReadP end -> ReadP [a]
  
  -- * Running a parser
  ReadS,      -- :: *; = String -> [(a,String)]
  readP_to_S, -- :: ReadP a -> ReadS a
  readS_to_P, -- :: ReadS a -> ReadP a
  
  -- * Properties
  -- $properties
  )
 where

import Prelude hiding (ReadS)
import Control.Monad( MonadPlus(..), liftM2 )
import Data.HeadTail(HeadTail(..))

--import GHC.List ( replicate )
--import GHC.Base

import Data.Char( isSpace )

infixr 5 +++, <++

------------------------------------------------------------------------
-- ReadS

-- | A parser for a type @a@, represented as a function that takes a
-- 'String' and returns a list of possible parses as @(a,'String')@ pairs.
--
-- Note that this kind of backtracking parser is very inefficient;
-- reading a large structure may be quite slow (cf 'ReadP').
type ReadS s a = s -> [(a,s)]

-- ---------------------------------------------------------------------------
-- The P type
-- is representation type -- should be kept abstract

data P s c a
  = Get (c -> P s c a)
  | Look (s -> P s c a)
  | Fail
  | Result a (P s c a)
  | Final [(a,s)] -- invariant: list is non-empty!

-- Monad, MonadPlus

instance HeadTail s c => Monad (P s c) where
  return x = Result x Fail

  (Get f)      >>= k = Get (\c -> f c >>= k)
  (Look f)     >>= k = Look (\s -> f s >>= k)
  Fail         >>= _ = Fail
  (Result x p) >>= k = k x `mplus` (p >>= k)
  (Final r)    >>= k = final [ys' | (x,s) <- r, ys' <- run (k x) s]

  fail _ = Fail

instance HeadTail s c => MonadPlus (P s c) where
  mzero = Fail

  -- most common case: two gets are combined
  Get f1     `mplus` Get f2     = Get (\c -> f1 c `mplus` f2 c)
  
  -- results are delivered as soon as possible
  Result x p `mplus` q          = Result x (p `mplus` q)
  p          `mplus` Result x q = Result x (p `mplus` q)

  -- fail disappears
  Fail       `mplus` p          = p
  p          `mplus` Fail       = p

  -- two finals are combined
  -- final + look becomes one look and one final (=optimization)
  -- final + sthg else becomes one look and one final
  Final r    `mplus` Final t    = Final (r ++ t)
  Final r    `mplus` Look f     = Look (\s -> Final (r ++ run (f s) s))
  Final r    `mplus` p          = Look (\s -> Final (r ++ run p s))
  Look f     `mplus` Final r    = Look (\s -> Final (run (f s) s ++ r))
  p          `mplus` Final r    = Look (\s -> Final (run p s ++ r))

  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look f     `mplus` Look g     = Look (\s -> f s `mplus` g s)
  Look f     `mplus` p          = Look (\s -> f s `mplus` p)
  p          `mplus` Look f     = Look (\s -> p `mplus` f s)

-- ---------------------------------------------------------------------------
-- The ReadP type

newtype ReadP s c a = R (forall b . (a -> P s c b) -> P s c b)

-- Functor, Monad, MonadPlus

instance Functor (ReadP s c) where
  fmap h (R f) = R (\k -> f (k . h))

instance Monad (ReadP s c) where
  return x  = R (\k -> k x)
  fail _    = R (\_ -> Fail)
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

instance HeadTail s c => MonadPlus (ReadP s c) where
  mzero = pfail
  mplus = (+++)

-- ---------------------------------------------------------------------------
-- Operations over P

final :: [(a,s)] -> P s c a
-- Maintains invariant for Final constructor
final [] = Fail
final r  = Final r

run :: HeadTail s c => P s c a -> ReadS s a
run (Get f)      s = maybe [] (\ (c,s') -> run (f c) s') (headTail s)
run (Look f)     s = run (f s) s
run (Result x p) s = (x,s) : run p s
run (Final r)    _ = r
run _            _ = []

-- ---------------------------------------------------------------------------
-- Operations over ReadP

get :: ReadP s c c
-- ^ Consumes and returns the next character.
--   Fails if there is no input left.
get = R Get

look :: ReadP s c s
-- ^ Look-ahead: returns the part of the input that is left, without
--   consuming it.
look = R Look

pfail :: ReadP s c a
-- ^ Always fails.
pfail = R (\_ -> Fail)

(+++) :: HeadTail s c => ReadP s c a -> ReadP s c a -> ReadP s c a
-- ^ Symmetric choice.
R f1 +++ R f2 = R (\k -> f1 k `mplus` f2 k)

(<++) :: HeadTail s c => ReadP s c a -> ReadP s c a -> ReadP s c a
-- ^ Local, exclusive, left-biased choice: If left parser
--   locally produces any result at all, then right parser is
--   not used.
R f <++ q =
  do s <- look
     probe (f return) s (0::Int)
 where
  probe (Get f')       s n = maybe q (\ (c,s') -> 
                                       n `seq` probe (f' c) s' (n+1)) 
                                   (headTail s)
  probe (Look f')      s n = probe (f' s) s n
  probe p@(Result _ _) _ n = discard n >> R (p >>=)
  probe (Final r)      _ _ = R (Final r >>=)
  probe _              _ _ = q

  discard 0 = return ()
  discard n  = get >> discard (n-1)

gather :: HeadTail s c => ReadP s c a -> ReadP s c ([c], a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact characters read.
--   IMPORTANT NOTE: 'gather' gives a runtime error if its first argument
--   is built using any occurrences of readS_to_P. 
gather (R m) =
  R (\k -> gath id (m (\a -> return (\s -> k (s,a)))))  
 where
  gath l (Get f)      = Get (\c -> gath (l.(c:)) (f c))
  gath _ Fail         = Fail
  gath l (Look f)     = Look (\s -> gath l (f s))
  gath l (Result k p) = k (l []) `mplus` gath l p
  gath _ (Final _)    = error "do not use readS_to_P in gather!"

-- ---------------------------------------------------------------------------
-- Derived operations

satisfy :: HeadTail s c => (c -> Bool) -> ReadP s c c
-- ^ Consumes and returns the next character, if it satisfies the
--   specified predicate.
satisfy p = do c <- get; if p c then return c else pfail

char :: (Eq c, HeadTail s c) => c -> ReadP s c c
-- ^ Parses and returns the specified character.
char c = satisfy (c ==)

string :: (Eq c, HeadTail s c) => [c] -> ReadP s c [c]
-- ^ Parses and returns the specified string.
string this = do s <- look; scan this (headTail s)
 where
  scan []     _                      = do return this
  scan (x:xs) (Just (y,ys)) | x == y = do _ <- get; scan xs (headTail ys)
  scan _      _                      = do pfail

munch :: (Eq c, HeadTail s c) => (c -> Bool) -> ReadP s c [c]
-- ^ Parses the first zero or more characters satisfying the predicate.
munch p =
  do s <- look
     scan (headTail s)
 where
  scan (Just (c,cs)) | p c = do _ <- get; s <- scan (headTail cs); return (c:s)
  scan _                   = do return []

munch1 :: (Eq c, HeadTail s c) => (c -> Bool) -> ReadP s c [c]
-- ^ Parses the first one or more characters satisfying the predicate.
munch1 p =
  do c <- get
     if p c then do s <- munch p; return (c:s) else pfail

choice :: HeadTail s c => [ReadP s c a] -> ReadP s c a
-- ^ Combines all parsers in the specified list.
choice []     = pfail
choice [p]    = p
choice (p:ps) = p +++ choice ps

skipSpaces :: HeadTail s Char => ReadP s Char ()
-- ^ Skips all whitespace.
skipSpaces =
  do s <- look
     skip (headTail s)
 where
  skip (Just (c,s)) | isSpace c = do _ <- get; skip (headTail s)
  skip _                        = do return ()

count :: Int -> ReadP s c a -> ReadP s c [a]
-- ^ @count n p@ parses @n@ occurrences of @p@ in sequence. A list of
--   results is returned.
count n p = sequence (replicate n p)

between :: ReadP s c open -> ReadP s c close -> ReadP s c a -> ReadP s c a
-- ^ @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between open close p = do _ <- open
                          x <- p
                          _ <- close
                          return x

option :: HeadTail s c => a -> ReadP s c a -> ReadP s c a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p +++ return x

optional :: HeadTail s c => ReadP s c a -> ReadP s c ()
-- ^ @optional p@ optionally parses @p@ and always returns @()@.
optional p = (p >> return ()) +++ return ()

many :: HeadTail s c => ReadP s c a -> ReadP s c [a]
-- ^ Parses zero or more occurrences of the given parser.
many p = return [] +++ many1 p

many1 :: HeadTail s c => ReadP s c a -> ReadP s c [a]
-- ^ Parses one or more occurrences of the given parser.
many1 p = liftM2 (:) p (many p)

skipMany :: HeadTail s c => ReadP s c a -> ReadP s c ()
-- ^ Like 'many', but discards the result.
skipMany p = many p >> return ()

skipMany1 :: HeadTail s c => ReadP s c a -> ReadP s c ()
-- ^ Like 'many1', but discards the result.
skipMany1 p = p >> skipMany p

sepBy :: HeadTail s c => ReadP s c a -> ReadP s c sep -> ReadP s c [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: HeadTail s c => ReadP s c a -> ReadP s c sep -> ReadP s c [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endBy :: HeadTail s c => ReadP s c a -> ReadP s c sep -> ReadP s c [a]
-- ^ @endBy p sep@ parses zero or more occurrences of @p@, separated and ended
--   by @sep@.
endBy p sep = many (do x <- p ; _ <- sep ; return x)

endBy1 :: HeadTail s c => ReadP s c a -> ReadP s c sep -> ReadP s c [a]
-- ^ @endBy p sep@ parses one or more occurrences of @p@, separated and ended
--   by @sep@.
endBy1 p sep = many1 (do x <- p ; _ <- sep ; return x)

chainr :: HeadTail s c => 
          ReadP s c a -> ReadP s c (a -> a -> a) -> a -> ReadP s c a
-- ^ @chainr p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /right/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainr p op x = chainr1 p op +++ return x

chainl :: HeadTail s c => 
          ReadP s c a -> ReadP s c (a -> a -> a) -> a -> ReadP s c a
-- ^ @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl p op x = chainl1 p op +++ return x

chainr1 :: HeadTail s c => 
           ReadP s c a -> ReadP s c (a -> a -> a) -> ReadP s c a
-- ^ Like 'chainr', but parses one or more occurrences of @p@.
chainr1 p op = scan
  where scan   = p >>= rest
        rest x = do f <- op
                    y <- scan
                    return (f x y)
                 +++ return x

chainl1 :: HeadTail s c =>
           ReadP s c a -> ReadP s c (a -> a -> a) -> ReadP s c a
-- ^ Like 'chainl', but parses one or more occurrences of @p@.
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 +++ return x

manyTill :: HeadTail s c => ReadP s c a -> ReadP s c end -> ReadP s c [a]
-- ^ @manyTill p end@ parses zero or more occurrences of @p@, until @end@
--   succeeds. Returns a list of values returned by @p@.
manyTill p end = scan
  where scan = (end >> return []) <++ (liftM2 (:) p scan)

-- ---------------------------------------------------------------------------
-- Converting between ReadP and Read

readP_to_S :: HeadTail s c => ReadP s c a -> ReadS s a
-- ^ Converts a parser into a Haskell ReadS-style function.
--   This is the main way in which you can \"run\" a 'ReadP' parser:
--   the expanded type is
-- @ readP_to_S :: ReadP s c a -> s c -> [(a,s c)] @
readP_to_S (R f) = run (f return)

readS_to_P :: HeadTail s c => ReadS s a -> ReadP s c a
-- ^ Converts a Haskell ReadS-style function into a parser.
--   Warning: This introduces local backtracking in the resulting
--   parser, and therefore a possible inefficiency.
readS_to_P r =
  R (\k -> Look (\s -> final [bs'' | (a,s') <- r s, bs'' <- run (k a) s']))

-- ---------------------------------------------------------------------------
-- QuickCheck properties that hold for the combinators

{- $properties
The following are QuickCheck specifications of what the combinators do.
These can be seen as formal specifications of the behavior of the
combinators.

We use bags to give semantics to the combinators.

>  type Bag a = [a]

Equality on bags does not care about the order of elements.

>  (=~) :: Ord a => Bag a -> Bag a -> Bool
>  xs =~ ys = sort xs == sort ys

A special equality operator to avoid unresolved overloading
when testing the properties.

>  (=~.) :: Bag (Int,String) -> Bag (Int,String) -> Bool
>  (=~.) = (=~)

Here follow the properties:

>  prop_Get_Nil =
>    readP_to_S get [] =~ []
>
>  prop_Get_Cons c s =
>    readP_to_S get (c:s) =~ [(c,s)]
>
>  prop_Look s =
>    readP_to_S look s =~ [(s,s)]
>
>  prop_Fail s =
>    readP_to_S pfail s =~. []
>
>  prop_Return x s =
>    readP_to_S (return x) s =~. [(x,s)]
>
>  prop_Bind p k s =
>    readP_to_S (p >>= k) s =~.
>      [ ys''
>      | (x,s') <- readP_to_S p s
>      , ys''   <- readP_to_S (k (x::Int)) s'
>      ]
>
>  prop_Plus p q s =
>    readP_to_S (p +++ q) s =~.
>      (readP_to_S p s ++ readP_to_S q s)
>
>  prop_LeftPlus p q s =
>    readP_to_S (p <++ q) s =~.
>      (readP_to_S p s +<+ readP_to_S q s)
>   where
>    [] +<+ ys = ys
>    xs +<+ _  = xs
>
>  prop_Gather s =
>    forAll readPWithoutReadS $ \p -> 
>      readP_to_S (gather p) s =~
>	 [ ((pre,x::Int),s')
>	 | (x,s') <- readP_to_S p s
>	 , let pre = take (length s - length s') s
>	 ]
>
>  prop_String_Yes this s =
>    readP_to_S (string this) (this ++ s) =~
>      [(this,s)]
>
>  prop_String_Maybe this s =
>    readP_to_S (string this) s =~
>      [(this, drop (length this) s) | this `isPrefixOf` s]
>
>  prop_Munch p s =
>    readP_to_S (munch p) s =~
>      [(takeWhile p s, dropWhile p s)]
>
>  prop_Munch1 p s =
>    readP_to_S (munch1 p) s =~
>      [(res,s') | let (res,s') = (takeWhile p s, dropWhile p s), not (null res)]
>
>  prop_Choice ps s =
>    readP_to_S (choice ps) s =~.
>      readP_to_S (foldr (+++) pfail ps) s
>
>  prop_ReadS r s =
>    readP_to_S (readS_to_P r) s =~. r s
-}
