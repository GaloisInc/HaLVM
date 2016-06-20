import           Control.Concurrent(threadDelay)
import           Control.Concurrent.MVar(MVar, newMVar, modifyMVar_, readMVar)
import           Control.Monad(forever, replicateM, forM_)
import           Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.List(intercalate)
import           Data.Word(Word)
import           Hypervisor.Console(Console, initXenConsole, writeConsole)
import           Hypervisor.XenStore(initXenStore)
import           Numeric(showHex)
import           XenDevice.NIC(listNICs, openNIC, setReceiveHandler)

main :: IO ()
main =
  do con <- initXenConsole
     replicateM 10 (do writeConsole con "I'm alive!\n"
                       threadDelay 1000000)
     xs <- initXenStore
     writeConsole con "XenStore initialized.\n"
     threadDelay 1000000
     nics <- listNICs xs
     threadDelay 1000000
     writeConsole con ("Found " ++ show (length nics) ++ "NICs:\n")
     forM_ nics $ \ d -> writeConsole con ("   " ++ d ++ "\n")
     threadDelay 1000000
     case nics of
       [] -> do writeConsole con ("BUG: No NICs found!\n")
                threadDelay 1000000
       (mac:_) ->
         do nic <- openNIC xs mac
            writeConsole con ("Opened device with MAC " ++ mac ++ "\n")
            threadDelay 1000000
            mv <- newMVar 0
            setReceiveHandler nic (handlePacket mv con)
            writeConsole con ("Set receive handler.\n")
            forever $ do count <- readMVar mv
                         writeConsole con ("Received " ++ show count ++ " packets.\n")
                         threadDelay (5 * 1000000)

handlePacket :: MVar Word -> Console -> ByteString -> IO ()
handlePacket countMV con bstr =
  modifyMVar_ countMV $ \ count ->
    do let (dest, rest)  = L.splitAt 6 bstr
           (src,  rest') = L.splitAt 6 rest
           etype         = L.take 2 rest'
       writeEtherAddress con src
       writeConsole      con " --> "
       writeEtherAddress con dest
       writeConsole      con (" (ethertype " ++ showHex (toEType etype) "" ++ ")\n")
       return (count + 1)
 where
  toEType :: ByteString -> Int
  toEType bs =
    let [b1, b2] = L.unpack bs
    in (fromIntegral b1 * 256) + fromIntegral b2
  --
  writeEtherAddress con bs = do
    let str = intercalate ":" (map toByte (L.unpack bs))
    writeConsole con str
  toByte x =
    let res = showHex x ""
    in if length res == 1 then '0':res else res

