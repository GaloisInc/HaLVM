-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
-- | Various utilities used in the network protocol stack modules
module Device.Network.API.Utils where

import Device.Bits

import Data.List(unfoldr)
import Data.Array.IArray
import Control.Concurrent

--class Functor f => Sequence f where sequence :: Monad m => f (m a) -> m (f a)
class Functor f => Container f where contents :: f a -> a

replace :: Functor f => f b -> a -> f a
replace x b = fmap (const b) x

emap :: (Functor f, Container c) => (b -> f a) -> c b -> f (c a)
emap f p= fmap (replace p) (f (contents p))

emap2 :: (Container d, Container c, Functor f) => 
         (b -> f a) -> d (c b) -> f (d (c a))
emap2 f = emap (emap f)

lift :: (Functor f, Container c) => c (f a) -> f (c a)
lift p = fmap (replace p) (contents p)

lift2 :: (Container c, Container d, Functor f) => c (d (f a)) -> f (c (d a))
lift2 p = lift (fmap lift p)

-------------------------------------------------------------------------------

doReq :: Chan b -> ((a -> IO ()) -> b) -> IO a
doReq reqChan req =
   do ans <- newEmptyMVar
      writeChan reqChan (req (putMVar ans))
      takeMVar ans

{- -- If there is a timeout, m is still completed and the result is ignored...
timeout t m =
  do c <- newChan
     fork $ do delay t ; writeChan c Nothing
     fork $ writeChan c . Just =<< m
     readChan c
-}
--------------------------------------------------------------------------------

foldlArray             :: (IArray arr elem, Ix ix, Enum ix)
                       => arr ix elem -> (elem -> a -> a) -> a -> a
foldlArray arr add zero = loop mn zero
  where
  (mn,mx)               = bounds arr
  loop ix tot       
    | ix <= mx          = loop (succ ix) (add (arr ! ix) tot)
    | otherwise         = tot

-- | The number of elements in an array
arraySize :: (Num t, Ix t, IArray a e) => a t e -> t
arraySize a             = mx - mn + 1
  where (mn,mx)             = bounds a


{-
checksum               :: (IArray arr Word16, Ix ix, Enum ix) => arr ix Word16 -> Word16
checksum arr            = let total   = foldlArray arr add (0 :: Word32)
                              add x t = fromIntegral x + t
                          in complement ((total .!. 1) + (total .!. 0))
-}
    

-- | TCP\/IP 16-bit checksums
checksum                 :: [Word16] -> Word16
checksum ws               = let total = sum (map fromIntegral ws) :: Word32
                            in complement (fromIntegral total + fromIntegral (total `shiftR` 16))



-- | Split a list into subcomponents of length 2.
-- The first argument is what to append in case the list is of odd length.
pairs                    :: a -> [a] -> [[a]]
pairs a                   = unfoldr mk
  where
  mk (x:y:zs)             = Just ([x,y],zs)
  mk [x]                  = Just ([x,a],[])
  mk []                   = Nothing


bytes_to_words_big       :: [Word8] -> [Word16]
bytes_to_words_big        = map catBits . pairs 0 

bytes_to_words_lil       :: [Word8] -> [Word16]
bytes_to_words_lil        = map (catBits . reverse) . pairs 0 

words_to_bytes_big       :: [Word16] -> [Word8]
words_to_bytes_big ws     = concat [ [w .!. 1, w .!. 0] | w <- ws ]

words_to_bytes_lil       :: [Word16] -> [Word8]
words_to_bytes_lil ws     = concat [ [w .!. 0, w .!. 1] | w <- ws ]
