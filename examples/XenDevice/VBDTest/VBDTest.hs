{-# LANGUAGE ScopedTypeVariables #-}
-- An example/test using the low-level block driver.
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--

import Control.Exception(assert)
import Control.Monad
import Data.List
import Data.Word
import Foreign.Storable
import Hypervisor.Debug
import Hypervisor.Kernel
import Hypervisor.Memory
import System.Time
import XenDevice.Console
import XenDevice.Disk
import XenDevice.Xenbus
import TestOpts


main :: IO ()
main = halvm_kernel_daemon [ dConsole, dDisks, dXenbus ] main'

main' :: [String] -> IO ()
main' args = do
  writeDebugConsole "Starting block test.\n"
  pDisks <- potentialDisks
  buffer <- allocateBuffer $ 128 * 1024
  checkBuffer <- allocateBuffer $ 128 * 1024
  mapM_ (runTests numTests buffer checkBuffer) pDisks
  where
    allocateBuffer :: Int -> IO [VPtr a]
    allocateBuffer x
      | x == 0    = return []
      | x > 0     = do
        rest <- allocateBuffer $ x - 4096
        page <- allocPage
        return (page:rest)
      | otherwise = fail "Internal error."

    -- FIXME: is 9 the max?
    numTests :: Int
    numTests =
      let opt_len = length opt
      in
        case args of
          [arg] | take opt_len arg == opt -> read $ drop opt_len arg
          _                               -> defaultNumTests


runTests :: Int -> [VPtr a] -> [VPtr a] -> String -> IO ()
runTests numTests buf1 buf2 name = do
  mdisk <- initializeDisk name
  case mdisk of
    Just disk -> do
      printDiskInfo disk
      forM [ round (2 ^^ block) * 512
           | block <- reverse [ 0 .. numTests - 1 ]
           ] $
        runWriteReadTest buf1 buf2 disk
      report "Done!\n"
      {-
      runWriteReadTest buf1 buf2 (128 * 1024) disk
      runWriteReadTest buf1 buf2 (64 * 1024)  disk
      runWriteReadTest buf1 buf2 (32 * 1024)  disk
      runWriteReadTest buf1 buf2 (16 * 1024)  disk
      runWriteReadTest buf1 buf2 (8 * 1024)   disk
      runWriteReadTest buf1 buf2 (4 * 1024)   disk
      runWriteReadTest buf1 buf2 (2 * 1024)   disk
      runWriteReadTest buf1 buf2 (1 * 1024)   disk
      runWriteReadTest buf1 buf2 512          disk
      -}
    Nothing -> 
      report $ "Disk " ++ name ++ " failed to initialize.\n"

printDiskInfo :: Disk -> IO ()
printDiskInfo disk = do
  report $ unlines 
    [ "\nDisk " ++ diskName disk ++ ":"
    , "  read-only: " ++ show (diskIsReadOnly disk)
    , "  removable: " ++ show (diskIsRemovable disk)
    , "  cdrom: " ++ show (diskIsCDRom disk)
    , "  bytes per sector: " ++ show (diskBytesPerSector disk)
    , "  sectors: " ++ show (diskSectors disk)
    ]

runWriteReadTest :: [VPtr a] -> [VPtr a] -> Disk -> Int -> IO ()
runWriteReadTest buffer1 buffer2 disk blockSize = do
  let bps       = diskBytesPerSector disk
      numSecs   = diskSectors disk
      bsizeWord = if blockSize >= 1024
                     then show (blockSize `div` 1024) ++ "K"
                     else show blockSize ++ "B"
      diskSize  = bps * fromIntegral numSecs
      numPages  = max (fromIntegral blockSize `div` 4096) 1
  report $ "Starting " ++ bsizeWord ++ " sync test "
  runTest (take numPages buffer1) (take numPages buffer2) $
    generateToDos bps (fromIntegral blockSize) diskSize
  where
    runTest :: [VPtr a] -> [VPtr a] -> [(Word64, Word32, Bool)] -> IO ()
    runTest _buf1 _buf2 []                         =
      report " PASSED\n"
    runTest  buf1  buf2 ((sector, size, dot):rest) = do
      randomizePages buf1
      copyPages buf1 buf2
      sameAfterCopy <- comparePages buf1 buf2
      assert sameAfterCopy $ return ()
      assert (size == fromIntegral blockSize) $ return ()
      writeResp <- writeBytes disk sector size buf1
      case writeResp of
        DROK -> do
          clearPages buf1 blockSize
          readResp <- readBytes disk sector size buf1
          case readResp of
            DROK -> do
              sameAfterReadWrite <- comparePages buf1 buf2
              if sameAfterReadWrite
                 then do
                   when dot $ report "."
                   runTest buf1 buf2 rest
                 else
                   report "FAILED: RD/WR Compare\n"
            _ ->
              report $ "FAILED: RD: " ++ show readResp ++ "\n"
        _ ->
          report $ "FAILED: WR: " ++ show writeResp ++ "\n"


generateToDos :: Word32 -> Word64 -> Word32 -> [(Word64, Word32, Bool)]
generateToDos secSize blockSize diskSize =
  zipWith (\ (x, y) ind -> (x, y, (ind `mod` dotEvery) == 0)) base digits
  where
    base     = unfoldr buildtodos diskSize
    digits   = iterate (\ x -> x + 1) 0
    dotEvery = length base `div` 16

    buildtodos :: Word32 -> Maybe ((Word64, Word32), Word32)
    buildtodos 0    = Nothing
    buildtodos left =
      Just ((sector, fromIntegral blockSize), newsize)
      where
        byteIndex         = diskSize - left
        (sector::Word64)  = fromIntegral $ byteIndex `div` secSize
        (newsize::Word32) = left - fromIntegral blockSize


clearPages :: [VPtr a] -> Int -> IO ()
clearPages pages blockSize =
  mapM_ (clearPage 0 $ min blockSize 4096) pages
  where
    clearPage :: Int -> Int -> VPtr a -> IO ()
    clearPage off maxp ptr | off == maxp = return ()
                           | otherwise   = do
      pokeByteOff ptr off (0::Word64)
      clearPage (off + 8) maxp ptr

randomizePages :: [VPtr a] -> IO ()
randomizePages pages =
  mapM_ (randomizePage 4096) pages
  where
    randomizePage :: Int -> VPtr a -> IO ()
    randomizePage 0 _   = return ()
    randomizePage x ptr = do
      TOD a b <- getClockTime
      let (rn::Word32) = fromIntegral $ a + b
      pokeByteOff ptr (4096 - x) rn
      randomizePage (x - 4) ptr

copyPages :: [VPtr a] -> [VPtr a] -> IO ()
copyPages buf1 buf2 =
  zipWithM_ (copyPage 4096) buf1 buf2 
  where
    copyPage :: Int -> VPtr a -> VPtr a -> IO ()
    copyPage 0 _fptr _tptr = return ()
    copyPage x  fptr  tptr = do
      (cur::Word64) <- peekByteOff fptr (4096 - x)
      pokeByteOff tptr (4096 - x) cur
      copyPage (x - 8) fptr tptr

comparePages :: [VPtr a] -> [VPtr a] -> IO Bool
comparePages buf1 buf2 = do
  liftM (all id) $ zipWithM (comparePage 4096) buf1 buf2
  where
    comparePage :: Int -> VPtr a -> VPtr a -> IO Bool
    comparePage 0 _ptr1 _ptr2 = return True
    comparePage x  ptr1  ptr2 = do
      (val1::Word64) <- peekByteOff ptr1 (4096 - x)
      (val2::Word64) <- peekByteOff ptr2 (4096 - x)
      if val1 == val2 
         then comparePage (x - 8) ptr1 ptr2
         else do
           writeDebugConsole $ unwords
             [ show ptr1, "!=", show ptr2, "@", show x ++ ":"
             , show val1, "!=", show val2 ++ "\n"
             ]
           return False

report :: String -> IO ()
report s = do
  writeConsole s
  writeDebugConsole s
 
