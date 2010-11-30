
module TestOpts where

opt :: String
opt = "numtests="

-- FIXME: MAGIC NUMBER ALERT! Calculated from 128 * 1024 (max block from
-- original formulation) == 2 ^ 9 * 512 (used in new formulation).  But is this
-- an arbitrary or an actual max for this test?
defaultNumTests :: Int
defaultNumTests = 9

