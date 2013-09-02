import Communication.IVC(InOutChannel)
import qualified Communication.IVC as IVC
import Communication.Rendezvous
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy(unpack)
import Data.Char
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore
import Prelude hiding (getLine)

newtype Msg = Msg String

instance Binary Msg where
  get = (Msg . map chr . map fromIntegral . unpack) `fmap` getLazyByteStringNul
  put (Msg s) = mapM_ put (map word8ify s) >> putWord8 0
   where
    word8ify :: Char -> Word8
    word8ify = fromIntegral . ord

accept :: XenStore -> IO (InOutChannel Msg Msg)
(_, accept) = peerConnection "rot13" (0.5, 1)

main :: IO ()
main = do
  writeDebugConsole "'Terminal' starting.\n"
  con <- initXenConsole
  xs <- initXenStore
  writeDebugConsole "Building connection.\n"
  ch <- accept xs
  writeDebugConsole "Connection established.\n"
  writeConsole con "Hello! This is your fabulous ROT13 terminal.\n"
  writeConsole con "  - Type a message, and it will be encrypted for you.\n"
  writeConsole con "  - Finish by entering a blank line.\n"
  runPrompt con ch

runPrompt :: Console -> InOutChannel Msg Msg -> IO ()
runPrompt con ch = do
  writeConsole con "> "
  msg <- getLine con
  IVC.put ch (Msg msg)
  unless (all isSpace msg) $ do
    Msg rsp <- IVC.get ch
    writeConsole con ("Encoded result: " ++ rsp ++ "\n")
    runPrompt con ch

getLine :: Console -> IO String
getLine con = do
  nextC <- readConsole con 1
  writeConsole con nextC
  case nextC of
    "\r" -> writeConsole con "\n" >> return ""
    [x]  -> (x:) `fmap` getLine con
    _    -> fail "More than one character back?"


