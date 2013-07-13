
module Dirty where

isDirty :: String -> Bool
isDirty w = {- map toLower -} w `elem` dirty

dirty :: [String]
dirty =
  [ "dirty"
  , "evil"
  , "bad"
  ]

