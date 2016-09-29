import Control.Concurrent
import Control.Monad
import Data.ByteString.Lazy(ByteString, pack)
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore
import XenDevice.Disk

main :: IO ()
main = do
  writeDebugConsole "Starting system!\n"
  con <- initXenConsole
  writeConsole con "Starting Disk device tests.\n"
  xs <- initXenStore
  writeDebugConsole "XenStore initialized!\n"
  disks <- listDisks xs
  threadDelay (1000000)
  writeConsole con ("Found " ++ show (length disks) ++ " disks:\n")
  forM_ disks $ \ d -> writeConsole con ("   " ++ d ++ "\n")
  writeConsole con "\n"
  diskinfos <- zip disks `fmap` mapM (openDisk xs) disks
  forM_ diskinfos $ \ (dname, d) -> do
    writeConsole con ("Information about " ++ dname ++ ":\n")
    writeConsole con ("    # Sectors: " ++ show (diskSectors d) ++ "\n")
    writeConsole con ("    Sector size: " ++ show (diskSectorSize d) ++ "\n")
    writeConsole con ("    isReadOnly: " ++ show (isDiskReadOnly d) ++ "\n")
    writeConsole con ("    isDiskRemovable: " ++ show (isDiskRemovable d)++"\n")
    writeConsole con ("    diskSupportsBarrier: " ++
                      show (diskSupportsBarrier d) ++ "\n")
    writeConsole con ("    diskSupportsFlush: " ++
                      show (diskSupportsFlush d) ++ "\n")
    writeConsole con ("    diskSupportsDiscard: " ++
                      show (diskSupportsDiscard d) ++ "\n")
    writeConsole con "\n"
  case lookup "hdb" diskinfos of
    Just disk ->
      do writeConsole con "Verifying read-only disk 'hdb'\n"
         checkROSectors con disk 0 0 (diskSectors disk)
    Nothing ->
      writeConsole con "Could not find read-only disk 'hdb'!\n"
  case lookup "hda" diskinfos of
    Just disk -> do
      writeConsole con "Verifying read/write disk hda\n"
      checkRWBlocks con disk
    Nothing ->
      writeConsole con "Could not find read/write disk 'hdb'!\n"
  writeConsole con "Done!\n"

checkROSectors :: Console -> Disk -> Word -> Word8 -> Word -> IO ()
checkROSectors con disk cursec val endsec
  | cursec == endsec =
     writeConsole con ("  --> Verified " ++ show endsec ++ " sectors.\n")
  | otherwise = do
     nextSec <- readDisk disk (diskSectorSize disk) cursec
     let example = pack (replicate 512 val)
     unless (example == nextSec) $ do
       writeConsole con ("  --> Verification FAILED at sector " ++
                         show cursec ++ "\n")
       fail "Verification failed!"
     checkROSectors con disk (cursec + 1) (val + 1) endsec

checkRWBlocks :: Console -> Disk -> IO ()
checkRWBlocks con disk = do
  writeTest 0 0 (diskSectors disk)
  writeDebugConsole "Completed write portion.\n"
  diskWriteBarrier disk
  writeDebugConsole "Executed write barrier.\n"
  readTest 0 0 (diskSectors disk)
 where
  secsPerBlock = 8192 `div` diskSectorSize disk
  --
  writeTest x cur top
    | cur == top =
       writeConsole con ("  --> Wrote " ++ show top ++ " sectors\n")
    | otherwise = do
       let example = pack (replicate 8192 x)
       writeDisk disk example cur
       writeTest (x + 1) (cur + secsPerBlock) top
  --
  readTest x cur top
    | cur == top =
       writeConsole con ("  --> Verified " ++ show top ++ " sectors\n")
    | otherwise = do
       let example = pack (replicate 8192 x)
       bstr <- readDisk disk 8192 cur
       unless (example == bstr) $ do
         writeConsole con ("  --> Verification FAILED at sector " ++
                           show cur ++ "\n")
         writeDebugConsole ("Start of example: " ++ show (BS.take 16 example) ++ "\n")
         writeDebugConsole ("Start of block: " ++ show (BS.take 16 bstr) ++ "\n")
         writeDebugConsole ("lengths: " ++ show (BS.length example) ++ " / " ++ show (BS.length bstr) ++ "\n")
         writeDebugConsole ("Difference info: " ++ show (differenceInfo example bstr) ++ "\n")
         fail "Verification failed!"
       readTest (x + 1) (cur + secsPerBlock) top

differenceInfo :: ByteString -> ByteString -> (Int, String)
differenceInfo wrote read = go 0 wrote read
 where
  go x w r =
    case (BS.uncons w, BS.uncons r) of
      (Nothing, Nothing) -> (x, "PERFECT")
      (Just _,  Nothing) -> (x, "Read ended first.")
      (Nothing, Just _)  -> (x, "Wrote ended first.")
      (Just (wv, w'), Just (rv, r'))
         | wv /= rv      -> (x, "Difference: " ++ show (BS.take 16 w) ++ " vs "
                                ++ show (BS.take 16 r))
         | otherwise     -> go (x + 1) w' r'
