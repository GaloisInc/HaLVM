{-# LANGUAGE ImplicitParams #-}

add :: (?val :: Int) => Int -> Int
add x = x + ?val

add1 = let ?val = 1 in add
