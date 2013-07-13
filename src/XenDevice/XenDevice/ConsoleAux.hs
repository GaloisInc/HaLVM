-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
module XenDevice.ConsoleAux(getLnConsole, xlateInput, xlateOutput) where

import Data.List (isPrefixOf)

getLnConsole :: (Int -> IO String) -> (String -> IO ()) -> IO String
getLnConsole readConsole writeConsole = getln ""
  where getln l = 
          do [c] <- readConsole 1
             case c of
               '\n' -> do writeConsole [c]
                          return (reverse l)
               '\b' | null l -> getln l
                    | otherwise -> do writeConsole "\b \b"
                                      getln (drop 1 l)
               _    -> do writeConsole [c]
                          getln (c:l)

xlateInput :: String -> String -- convert CR's to LF's
xlateInput = subst "\r" "\n"

xlateOutput :: String -> String  -- insert CR's after LF's
xlateOutput = subst "\n" "\r\n"

-- Handy general utility
subst :: Eq a => [a] -> [a] -> [a] -> [a]
subst [] _ s = s
subst _ _ [] = []
subst u v xss@(_:_) | u `isPrefixOf` xss = v ++ subst u v (drop (length u) xss)
subst u v (x:xs) = x:(subst u v xs)
