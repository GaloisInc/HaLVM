-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Adam Wick <awick@galois.com>
-- BANNEREND
module Device.Screen(
         Screen
       , initRealScreen
       , initVirtualScreen
       , linkVirtualScreen
       , fill_box
       , placeCursor
       , putStr
       , putStrLn
       , putStrAt 
       , putStrCenter 
       )
 where

import Prelude hiding (putStrLn,putStr)

import Control.Concurrent
import Control.Concurrent.STM
import Data.Bits
import Data.ByteString.Internal(memcpy)
import Data.Char
import Data.Word
import Device.Screen.ColorAttribute
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Basics
import Hypervisor.IOPorts
import Hypervisor.Memory
import Hypervisor.Privileged
import System.IO.Unsafe(unsafePerformIO)

-- |An opaque structure describing a (physical or virtual) screen.
data Screen = Screen {
    video_mem  :: VPtr Word8
  , cursor     :: MVar (Int, Int)
  , scolumns   :: Int
  , slines     :: Int
  , real_scr   :: Bool
  , dirtyTV    :: TVar Bool
  }

-- |Initialize the real screen hooked up to the computer.
initRealScreen :: IO Screen
initRealScreen = do
  vm <- mapForeignMachineFrames (DomId 0) [toMFN 0xb8]
  setIOPrivilegeLevel 3
  curs <- newMVar (0,24)
  dTV <- newTVarIO False
  let scr = Screen {
              video_mem  = vm
            , cursor     = curs
            , scolumns   = 80
            , slines     = 25
            , real_scr   = True
            , dirtyTV    = dTV
            }
  _ <- forkIO $ screen_linker_thread scr
  scroll_screen scr
  -- enable the cursor
  out8 0x3d4 0x09 
  maxsl <- in8 0x3d5
  out8 0x3d4 0x0a
  out8 0x3d5 (maxsl .&. 0x1F)
  return scr

-- |Create a new, blank virtual screen.
initVirtualScreen :: IO Screen
initVirtualScreen = do
  vm <- allocPage
  curs <- newMVar (0,24)
  dTV <- newTVarIO False
  let scr = Screen {
              video_mem  = vm
            , cursor     = curs
            , scolumns   = 80
            , slines     = 25
            , real_scr   = False
            , dirtyTV    = dTV
            }
  fill_box scr (0,0) (79,24) (White `on` Black) ' '
  return scr

channel_link :: TChan (VPtr Word8, Int, Int, MVar (Int, Int), TVar Bool)
channel_link = unsafePerformIO $ newTChanIO

-- |Link a virtual screen to the physical screen. The two integer arguments
-- are the row to start the link and the row to end the link, inclusive. Data
-- from outside this box will be ignored.
linkVirtualScreen :: Screen -> Int -> Int -> IO ()
linkVirtualScreen scr sy ey
  | real_scr scr = fail "Attempt to link physical screen to itself."
  | otherwise    = atomically $ writeTChan channel_link (vm, sy, ey, cMV, dTV)
 where
  vm   = video_mem scr
  cMV  = cursor scr
  dTV  = dirtyTV scr

screen_linker_thread :: Screen -> IO ()
screen_linker_thread phys = do
  fakeTV <- newTVarIO False
  cMV <- newMVar (0,0)
  run_linker_thread (video_mem phys) (video_mem phys) 4096 cMV fakeTV
 where
  run_linker_thread dptr sptr size cMV alert = do
    res <- atomically (get_next_input alert)
    case res of
      -- Update the screen
      Right () -> do
        memcpy dptr sptr size
        (x,y) <- readMVar cMV
        update_cursor x y
        run_linker_thread dptr sptr size cMV alert
      Left (vm, sy, ey, cMV', alert') -> do
        let offset = sy * scolumns phys * 2
            size'  = fromIntegral $ (ey - sy + 1) * scolumns phys * 2
            dptr'  = video_mem phys `plusPtr` offset
            sptr'  = vm `plusPtr` offset
        run_linker_thread dptr' sptr' size' cMV' alert'
  --
  get_next_input alert = (Left `fmap` readTChan channel_link) `orElse` (do
    dirty_p <- readTVar alert
    if dirty_p
      then writeTVar alert False >> return (Right ())
      else retry)

-- |Place the cursor at the given point on the screen.
placeCursor :: Screen -> Int -> Int -> IO ()
placeCursor scr x y 
 | real_scr scr = modifyMVar_ (cursor scr) (\ _ -> return (x,y))
 | otherwise    = return ()

-- |Put a string to the given screen, with the given color attribute, at
-- the current cursor position.
putStr :: Screen -> ColorAttribute -> String -> IO ()
putStr scr ca str = modifyMVar (cursor scr) $ \ (sx, sy) -> do
  res@(ex, ey) <- run_write sx sy str
  update_cursor ex ey
  return (res, ())
 where 
  run_write x y [] = return (x, y)
  run_write _ y ('\r':rest) = run_write 0 y rest
  run_write _ y ('\n':rest) 
    | y == last_y = scroll_screen scr >> run_write 0 y rest
    | otherwise   = run_write 0 (y + 1) rest
  run_write x y cur@(c:rest)
    | x == scolumns scr && y == last_y = scroll_screen scr >> run_write 0 y cur
    | x == scolumns scr                = run_write 0 (y + 1) cur
    | otherwise                        = write_char scr x y ca c >>
                                         run_write (x + 1) y rest
  last_y = slines scr - 1

-- |As putStr, but adds a newline to the screen. 
putStrLn :: Screen -> ColorAttribute -> String -> IO ()
putStrLn scr ca str = putStr scr ca (str ++ "\n")

-- |Put a string in the center of the given line. It is an error to pass a
-- string longer than the width of the screen.
putStrCenter :: Screen -> Int -> ColorAttribute -> String -> IO ()
putStrCenter scr y ca str 
  | length str > scolumns scr = error "String too long to center"
  | otherwise                 = putStrAt scr x y ca str
 where x = (scolumns scr - length str) `div` 2

-- |Put a string at the given position on the screen.
putStrAt :: Screen -> Int -> Int -> ColorAttribute -> String -> IO ()
putStrAt _ _ _ _ [] = return ()
putStrAt scr x y ca (c:rest) = do
  write_char scr x y ca c
  putStrAt scr (x + 1) y ca rest

-- |Fill a box on the screen with the given color attribute and character.
-- The position arguments are the upper left and lower right corners of
-- the box.
fill_box :: Screen -> 
            (Int, Int) -> (Int, Int) -> 
            ColorAttribute -> Char -> 
            IO ()
fill_box scr (sx, sy) (ex, ey) ca c = mapM_ fill_line [sy .. ey]
 where fill_line y = mapM_ (\x->write_char scr x y ca c) [sx .. ex]

scroll_screen :: Screen -> IO ()
scroll_screen scr = do
  memmove (video_mem scr) from size
  fill_box scr (0, slines scr - 1) (scolumns scr - 1, slines scr - 1)
           (White `on` Black) ' '
 where
  from = video_mem scr `plusPtr` (scolumns scr * 2)
  size = fromIntegral $ (slines scr - 1) * scolumns scr * 2 

update_cursor :: Int -> Int -> IO ()
update_cursor x y = do
  let pos = (y * 80) + x
  out8 0x3D4 14
  out8 0x3D5 (fromIntegral $ pos `shiftR` 8)
  out8 0x3D4 15
  out8 0x3D5 (fromIntegral pos)

write_char :: Screen -> Int -> Int -> ColorAttribute -> Char -> IO ()
write_char scr x y ca c = do
  pokeByteOff (video_mem scr) offset c'
  pokeByteOff (castPtr $ video_mem scr) (offset + 1) ca
  atomically $ writeTVar (dirtyTV scr) True
 where
  offset = (y * 160) + (x * 2)
  c'::Word8 
  c' = fromIntegral $ ord c

foreign import ccall unsafe "memmove"
  memmove :: Ptr a -> Ptr a -> Int -> IO ()
