
module Common where


import Data.Word
import Data.Char
import Data.Serialize
import Control.Monad
import Numeric


data MAC = MAC Word8 Word8 Word8 Word8 Word8 Word8 deriving ( Eq )


instance Serialize MAC where 
  get = replicateM 6 get >>= (\ [a,b,c,d,e,f] -> return $ MAC a b c d e f)
  put (MAC a b c d e f) = mapM_ put [a,b,c,d,e,f]

instance Show MAC where
    show (MAC a b c d e f) = 
        (showHex a "") ++ ":" ++ (showHex b "") ++ ":" ++ 
        (showHex c "") ++ ":" ++ (showHex d "") ++ ":" ++ 
        (showHex e "") ++ ":" ++ (showHex f "")

instance Read MAC where
    readsPrec _ str = [(readMAC str, "")]

-- |A function for translating MAC addresses in standard string format
-- (00:11:22:33:44:55) into the internal MAC format.
readMAC :: String -> MAC
readMAC str = let [a1, a2, a3, a4, a5, a6] = map strToNum $ getStrs str
              in MAC (fromIntegral a1) (fromIntegral a2) (fromIntegral a3)
                     (fromIntegral a4) (fromIntegral a5) (fromIntegral a6)
 where 
   getStrs :: String -> [String]
   getStrs "" = [] -- If this looks familiar to "lines", it's because it is
   getStrs s = let (l, s') = break (== ':') s
	       in  l : case s' of
			 []   	 -> []
			 (_:s'') -> getStrs s''
   strToNum :: String -> Word8
   strToNum [a,b] = fromIntegral $ (digitToInt a * 16) + (digitToInt b)
   strToNum _ = error "Weird case in strToNum"
