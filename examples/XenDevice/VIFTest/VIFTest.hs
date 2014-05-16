import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.List
import Hypervisor.Console
import Hypervisor.XenStore
import Numeric
import XenDevice.NIC

main :: IO ()
main = do
  con <- initXenConsole
  xs <- initXenStore
  writeConsole con ("Starting NIC device tests.\n")
  nics <- listNICs xs
  threadDelay (1000000)
  writeConsole con ("Found " ++ show (length nics) ++ " NICs:\n")
  forM_ nics $ \ d -> writeConsole con ("   " ++ d ++ "\n")
  case nics of
    [] -> do writeConsole con ("BUG: No NICs found!\n")
             threadDelay 1000000
    (mac:_) -> do nic <- openNIC xs mac
                  writeConsole con ("Opened device with MAC " ++ mac ++ "\n")
                  mv <- newMVar ()
                  setReceiveHandler nic (handlePacket mv con)
                  writeConsole con ("Set receive handler. Stalling for ~10\n")
                  threadDelay (1 * 1000000)
                  let bstr = BS.pack [0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                                      0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                                      0x00, 0x4,  0xab, 0xab, 0xab, 0xab]
                  writeConsole con ("Sending packet.\n")
                  forM_ [0..90] $ \ _ ->
                    do sendPacket nic bstr
                       threadDelay 500000
                  threadDelay (90 * 1000000)
                  writeConsole con ("Exitting nicely.\n")

handlePacket :: MVar () -> Console -> BS.ByteString -> IO ()
handlePacket lock con bstr = do
  _ <- takeMVar lock
  let (dest,  rest)  = BS.splitAt 6 bstr
      (src,   rest') = BS.splitAt 6 rest
      etype          = BS.take 2 rest'
  writeEtherAddress con src
  writeConsole      con " --> "
  writeEtherAddress con dest
  writeConsole      con (" (ethertype " ++ showHex (toEType etype) "" ++ ")\n")
  putMVar lock ()
 where
  toEType :: BS.ByteString -> Int
  toEType bs =
    let [b1, b2] = BS.unpack bs
    in (fromIntegral b1 * 256) + fromIntegral b2
  --
  writeEtherAddress con bs = do
    let str = intercalate ":" (map toByte (BS.unpack bs))
    writeConsole con str
  toByte x =
    let res = showHex x ""
    in if length res == 1 then '0':res else res
