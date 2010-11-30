module Utils (
    -- * IO Helpers
    delay
  , putStr
  , putStrLn
  , print
  , header

    -- * Halfs Helpers
  , readFile
  , writeFile
  ) where

import Prelude hiding     ( putStr, putStrLn, print )

-- Libraries
import Control.Concurrent ( threadDelay )
import Data.Integral      ( INInt )
import Data.List          ( splitAt, genericLength )
import Halfs.Buffer       ( Buffer, buffToStrSize, strToBuff )
import XenDevice.Console  ( writeConsole )
import qualified HalfsIO


-- IO Helpers ------------------------------------------------------------------

delay :: IO ()
delay  = threadDelay 1000

putStr :: String -> IO ()
putStr  = writeConsole

putStrLn :: String -> IO ()
putStrLn str = putStr $ str ++ "\n"

print :: Show a => a -> IO ()
print = putStrLn . show

header :: String -> IO ()
header str = do
  putStrLn str
  putStrLn (replicate (length str) '-' ++ "\n")


-- Halfs Helpers ---------------------------------------------------------------

-- | Read file from the mounted filesystem present in StateHandle, using
--   Buffer, and reading INInt bytes at a time.
readFile :: HalfsIO.RdHandle -> Buffer -> INInt -> FilePath -> IO String
readFile rh buf len path = do
  fh <- HalfsIO.openRead rh path
  let loop = do
        rd   <- HalfsIO.read fh buf len
        text <- buffToStrSize (fromIntegral rd) buf
        if rd < len
          then return [text]
          else (text:) `fmap` loop
  blocks <- loop
  HalfsIO.closeRead fh
  return (concat blocks)


-- | Write the contents of a string to a FilePath.
writeFile :: HalfsIO.WtHandle -> Buffer -> INInt -> String -> FilePath -> IO ()
writeFile wh buf len text path = do
  fh <- HalfsIO.openWrite wh path
  let loop []   = return ()
      loop rest = do
        let (block,rest') = splitAt (fromIntegral len) rest
        strToBuff block buf
        HalfsIO.write fh buf (genericLength block)
        loop rest'
  loop text
  HalfsIO.closeWrite fh
