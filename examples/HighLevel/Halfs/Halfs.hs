{-# LANGUAGE OverloadedStrings  #-}
import System.Device.Memory
import System.Device.ST
import Control.Monad.ST (stToIO)
import System.Device.BlockDevice
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Hypervisor.Debug
import Data.Word
import XenDevice.Disk
import Hypervisor.XenStore
import Halfs.CoreAPI
import Halfs.Types
import Halfs.Classes
import Halfs.Errors
import Halfs.HalfsState
import Halfs.MonadUtils
import Halfs.Utils
import Halfs.Monad
import Halfs.SuperBlock (SuperBlock)
import Control.Monad.Reader
import System.FilePath
import Control.Concurrent
import Control.Exception
import Control.Monad
import Hypervisor.Console
import Hypervisor.ErrorCodes

import Prelude hiding (getLine)


{- ST
main :: IO ()
main = do
    mbBlockDevice <- stToIO $ newSTBlockDevice 1024 512 -- Initialize a block device as ST
    case mbBlockDevice of
        Just device -> do
            bstring <- stToIO $ do
                let wb = bdWriteBlock device
                wb 512 "hello world"
	        bstring <- bdReadBlock device $ 512
                return bstring
            writeDebugConsole $ "String here: " ++ (BS.unpack bstring)
        Nothing -> writeDebugConsole "device error"
-}

{- Memory
main :: IO ()
main = do
    mbBlockDevice <- newMemoryBlockDevice 1024 512 -- Initialize a block device as Memory
    case mbBlockDevice of
        Just device -> do
	    let wb = bdWriteBlock device
	    wb 512 "hello world"
	    bstring <- bdReadBlock device $ 512
	    writeDebugConsole $ "String here: " ++ (BS.unpack bstring)
	Nothing -> writeDebugConsole "device error"
-}

-- {- Disk
-- Create a new in-memory block device, with the given number of
-- sectors and sector size.

newDiskBlockDevice :: Disk -> IO (Maybe (BlockDevice IO))
newDiskBlockDevice disk = return $! Just BlockDevice {
        bdBlockSize  = fromIntegral $ diskSectorSize disk
      , bdNumBlocks  = fromIntegral $ diskSectors disk
      , bdReadBlock  = \sector -> do
            bl <- readDisk disk (fromIntegral $ diskSectorSize disk) (fromIntegral sector)
            return $ bLtoBS bl
      , bdWriteBlock = \sector bs -> writeDisk disk (bStoBL bs) $ fromIntegral sector
      , bdFlush      = flushDiskCaches disk
      , bdShutdown   = return ()
      }

bLtoBS :: BL.ByteString -> BS.ByteString
bLtoBS = BS.concat . BL.toChunks
bStoBL :: BS.ByteString -> BL.ByteString
bStoBL bs = BL.fromChunks [bs]

rwx = [Read, Write, Execute]
defaultPerm = FileMode rwx rwx rwx

main :: IO ()
main = do
    xs <- initXenStore
    con <- initXenConsole
    diskNames <- listDisks xs
    writeDebugConsole $ "Disks: " ++ show diskNames ++ "\n"
    case diskNames of
      (diskName:_) -> do
        disk <- openDisk xs diskName
--        mdiskBD <- newDiskBlockDevice disk -- This is buggy, I need to tune it when I got some extra time
        mdiskBD <- newMemoryBlockDevice 1024 1024
        case mdiskBD of
          Just diskBD -> do
            fsState <- mountFS diskBD
            
            feedback <- runHalfs fsState $ do
              mkdir "/" defaultPerm
              return "/"
            case feedback of
              Right fp -> repl xs con fp fsState
              Left err -> writeDebugConsole $ "Fail: " ++ show err ++ "\n"
            unmountInfo <- runHalfs fsState unmount
            case unmountInfo of
              Left err -> writeDebugConsole $ "Error in unmounting: " ++ show err
              Right () -> return ()
          Nothing -> writeDebugConsole "Error in initializing disk block device!"
      [] -> writeDebugConsole "No available disks!"

newFS :: (Monad m, HalfsCapable b t r l m) => BlockDevice m -> m SuperBlock
newFS diskBD = execNoEnv $ newfs diskBD 0 0 defaultPerm

mountFS :: (Monad m, HalfsCapable b t r l m) => BlockDevice m -> m (HalfsState b r l m)
mountFS diskBD = execNoEnv $ mount diskBD 0 0 defaultPerm

execNoEnv :: Monad m => HalfsM b r l m a -> m a
execNoEnv act = do
  runHalfsNoEnv act >>= \ea -> case ea of
    Left e  -> fail $ show e
    Right x -> return x

repl xs con here fsState = do
  me <- xsGetDomId xs
  writeConsole con ("Hello! This is an interactive Unix-like file-system shell for " ++
                    show me ++ "\n")
  writeConsole con ("Valid commands: quit, ls, cd, mkdir\n\n")
  writeDebugConsole "Starting interaction loop!\n"
  info <- runHalfs fsState $ loop con here
  return ()

loop con here = do
  lift $ writeConsole con (here ++ "> ")
  inquery <- lift $ getLine con
  case words inquery of
    ("quit":_) -> return ()
    ("ls"  :_) -> do
      handle <- openDir here
      dirInfo <- readDir handle
      lift $ writeConsole con $ printDir dirInfo ++ "\n"
      loop con here
    ("cd"  :x:_) -> do
      case x of
        ".." -> loop con (takeDirectory here)
        d    -> do
          handle <- openDir here
          dirInfo <- readDir handle
          if (filter (== d) $ map fst dirInfo) /= [] then
            loop con (here </> d)
            else do
                 lift $ writeConsole con "No such directory\n"
                 loop con here
    ("mkdir":x:_) -> do
      if x /= ".." then mkdir (here </> x) defaultPerm
        else lift $ writeConsole con "Invalid directory name\n"
      loop con here
    _ -> do
      lift $ writeConsole con "Unrecognized command\n"
      loop con here

getLine :: Console -> IO String
getLine con = do
  nextC <- readConsole con 1
  writeConsole con nextC
  case nextC of
    "\r" -> writeConsole con "\n" >> return ""
    [x]  -> (x:) `fmap` getLine con
    _    -> fail "More than one character back?"

printDir [] = ""
printDir (x:[]) = fst x
printDir (x:xs) = fst x ++ "\t" ++ printDir xs
