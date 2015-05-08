{-# LANGUAGE OverloadedStrings #-}
import System.Device.Memory
import System.Device.ST
import Control.Monad.ST (stToIO)
import System.Device.BlockDevice
import Data.ByteString.Char8 as BS
import Hypervisor.Debug

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

-- {- Memory
main :: IO ()
main = do
    mbBlockDevice <- newMemoryBlockDevice 1024 512 -- Initialize a block device as ST
    case mbBlockDevice of
        Just device -> do
	    let wb = bdWriteBlock device
	    wb 512 "hello world"
	    bstring <- bdReadBlock device $ 512
	    writeDebugConsole $ "String here: " ++ (BS.unpack bstring)
	Nothing -> writeDebugConsole "device error"

