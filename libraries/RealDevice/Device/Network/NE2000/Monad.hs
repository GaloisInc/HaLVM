-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
{-# OPTIONS -fglasgow-exts #-}
module Device.Network.NE2000.Monad 
  ( Page0(..), Page1(..), Page2(..), On, Off
  , NE2000, neReturn, neBind, neThen, neMap, neRun
  , neWhen, neWhenM, neMapM_

  , newChan, readChan, writeChan, emptyChan, putMVar
  , debug

  , getDMA, dmaTxArray, dmaRxArray

  -- Any page
  , getCmd
  , stop, start   
  , remoteRead, remoteWrite
  , txStart, txing
  , turnToPage
  , reset  

  -- Page 0
  , rxBufferStart, rxBufferEnd       
  , setBoundary, getBoundary      
  , txPage, txByteCount     
  , clearEvents, getEvents, ackEvent     
  , remoteAddress, remoteByteCount    
  , rxConfig, txConfig, dataConfig  
  , interruptMask 

  -- Page 1
  , setCurrent, getCurrent
  , physAddr, multicast    
  )
  where

import Device.Bits
import qualified Device.Network.API.Ethernet as Eth

import Control.Monad.Reader
import Data.Array.Unboxed
import qualified Control.Concurrent as Conc

import Hypervisor.IOPorts



--------------------------------------------------------------------------------
-- Level 0
--------------------------------------------------------------------------------

type Comp           = ReaderT ( Port              -- base register, 
                              , String -> IO ())  -- debug print
                      IO

getBase            :: Comp Port
getBase             = (\(x,_) -> x) `fmap` ask

run                :: Port -> (String -> IO ()) -> Comp a -> IO a
run port prt m      = runReaderT m (port, prt)

setReg             :: Word8 -> Word8 -> Comp ()
setReg reg val      = do base <- getBase
                         lift (out8 (base + fromIntegral reg) val)

getReg             :: Word8 -> Comp Word8
getReg reg          = do base <- getBase
                         lift (in8 (base + fromIntegral reg))

setDMA             :: Word16 -> NE2000 s s ()
setDMA val          = Do (do base <- getBase
                             lift (out16 (base + 0x10) val))

getDMA             :: NE2000 s s Word16
getDMA              = Do (do base <- getBase
                             lift (in16 (base + 0x10)))

-- | Write the contents of an array to the DMA port.
-- If the array is of odd length we transmit a 0 at the end.
dmaTxArray         :: UArray Int Word8 -> NE2000 s s ()
dmaTxArray a        = loop mn
  where loop n 
          | n > mx    = neReturn ()
          | n == mx   = setDMA ((0 :: Word8) `nextTo` (a ! n))
          | otherwise = let w = (a ! (n+1)) `nextTo` (a ! n)
                        in setDMA w `neThen` loop (n+2)
        (mn,mx)       = bounds a

-- | Get 'n' bytes from the NIC's buffer.
dmaRxArray         :: Int -> NE2000 s s (UArray Int Word8)
dmaRxArray n        = loop n [] `neBind` \bs -> 
                      neReturn (array (0,n-1) (zip [0..] bs))
  where loop 0 as   = neReturn (reverse as)
        loop 1 as   = getDMA `neBind` \w -> 
                      neReturn (reverse ((w .!. 0) : as))
        loop n' as  = getDMA `neBind` \w -> 
                      loop (n'-2) ((w .!. 1) : (w .!. 0) : as)


debug              :: String -> NE2000 s s ()
debug s             = Do (do (_,prt) <- ask
                             lift (prt s))



--------------------------------------------------------------------------------
-- Level 1
--------------------------------------------------------------------------------

-- The static card state consist of:
--   * the register page we are in
--   * if the card is on or off
-- 
-- NOTE: It is important that the card is accessed in 
-- a single threaded fashion for the type tr3ick to work.


data Page0          = Page0
data Page1          = Page1
data Page2          = Page2

class Page t where
  fromPage         :: t -> Word8

instance Page Page0 where fromPage _ = 0
instance Page Page1 where fromPage _ = 1
instance Page Page2 where fromPage _ = 2
  
data On 
data Off

newtype NE2000 s1 s2 a  = Do (Comp a)

neReturn           :: a -> NE2000 s t a
neReturn x          = Do (return x)

neBind             :: NE2000 s1 s2 a
                   -> (a -> NE2000 s2 s3 b)
                   -> NE2000 s1 s3 b
neBind (Do m) f     = Do (do a <- m
                             let Do m' = f a
                             m')

neThen             :: NE2000 s1 s2 a -> NE2000 s2 s3 b -> NE2000 s1 s3 b
neThen m1 m2        = m1 `neBind` \_ -> m2

neMap              :: (a -> b) -> NE2000 s1 s2 a -> NE2000 s1 s3 b
neMap f m           = m `neBind` \x -> neReturn (f x)

neMapM_            :: (a -> NE2000 s s b) -> [a] -> NE2000 s s ()
neMapM_ _ []        = neReturn ()
neMapM_ f (x:xs)    = f x `neThen` neMapM_ f xs

neRun                :: Port -> (String -> IO ()) -> 
                        NE2000 (Page0,Off) s2 a -> IO a
neRun port prt (Do m) = run port prt (setReg 0x00 0x21 >> m)

-- misc
neWhen             :: Bool -> NE2000 s1 s2 () -> NE2000 s1 s2 ()
neWhen b m          = if b then m else neReturn ()

neWhenM            :: NE2000 s1 s2 Bool -> NE2000 s2 s3 () -> NE2000 s1 s3 ()
neWhenM mb m        = mb `neBind` \b -> neWhen b m


-- | Should not exeute IO that can manipulate the card.
io                 :: IO a -> NE2000 s s a
io m                = Do (lift m)

emptyChan          :: Conc.Chan a -> NE2000 s s Bool
emptyChan r         = io (Conc.isEmptyChan r)

newChan            :: NE2000 s s (Conc.Chan a)
newChan             = io (Conc.newChan)

readChan           :: Conc.Chan a -> NE2000 s s a
readChan r          = io (Conc.readChan r)

writeChan          :: Conc.Chan a -> a -> NE2000 s s ()
writeChan r x       = io (Conc.writeChan r x)

putMVar            :: Conc.MVar a -> a -> NE2000 s s ()
putMVar r x         = io (Conc.putMVar r x)

getCmd              :: NE2000 (page,state) (page,state) Word8
getCmd              = Do $ getReg 0x00
                              
stop               :: NE2000 (page,state) (Page0,Off) ()
stop                = Do $ setReg 0x00 0x21

start              :: NE2000 (page,state) (Page0,On) ()
start               = Do $ setReg 0x00 0x22

remoteRead         :: NE2000 (page,state) (Page0,On) ()
remoteRead          = Do $ setReg 0x00 0x0A

remoteWrite        :: NE2000 (page,state) (Page0,On) ()
remoteWrite         = Do $ setReg 0x00 0x12

txStart            :: NE2000 (page,state) (Page0,On) ()
txStart             = Do $ setReg 0x00 0x26 -- 00 100 110


class TurnPage state where
  turnToPage       :: Page page => page -> NE2000 (page',state) (page,state) ()

instance TurnPage On where
  turnToPage x      = Do (setReg 0x00 ((fromPage x `shiftL` 6) .|. 0x22))

instance TurnPage Off where
  turnToPage  x     = Do (setReg 0x00 ((fromPage x `shiftL` 6) .|. 0x21))


txing              :: NE2000 s s Bool
txing               = Do (do cmd <- getReg 0x00
                             return (cmd `testBit` 2))
-- what type?
reset              :: ReaderT (Port, String -> IO ()) IO ()
reset               = setReg 0x1F =<< getReg 0x1F





rxBufferStart      :: Word8 -> NE2000 (Page0,Off) (Page0,Off) ()
rxBufferStart x     = Do (setReg 0x01 x)

rxBufferEnd        :: Word8 -> NE2000 (Page0,Off) (Page0,Off) ()
rxBufferEnd x       = Do (setReg 0x02 x)

setBoundary        :: Word8 -> NE2000 (Page0,state) (Page0,state) ()
setBoundary x       = Do (setReg 0x03 x)

getBoundary        :: NE2000 (Page0,state) (Page0,state) Word8
getBoundary         = Do (getReg 0x03)

txPage             :: Word8 -> NE2000 (Page0,state) (Page0,state) ()
txPage x            = Do (setReg 0x04 x)

txByteCount        :: Int -> NE2000 (Page0,state) (Page0,state) ()
txByteCount x       = Do (do setReg 0x05 (x .!. 0)
                             setReg 0x06 (x .!. 1))

clearEvents        :: NE2000 (Page0,state) (Page0,state) ()
clearEvents         = Do (setReg 0x07 0xFF)

getEvents          :: NE2000 (Page0,state) (Page0,state) Word8
getEvents           = Do (getReg 0x07)

-- ackEvent x: acknowledge event in bit x
-- XXX: add a type?
ackEvent           :: Int -> NE2000 (Page0,state) (Page0,state) ()
ackEvent x          = Do (setReg 0x07 (1 `shiftL` x))

remoteAddress      :: Word16 -> NE2000 (Page0,state) (Page0,state) ()
remoteAddress addr  = Do (do setReg 0x08 (addr .!. 0)
                             setReg 0x09 (addr .!. 1))

remoteByteCount    :: Int -> NE2000 (Page0,state) (Page0,state) ()
remoteByteCount len = Do (do setReg 0x0A (len .!. 0)  
                             setReg 0x0B (len .!. 1))

rxConfig           :: Word8 -> NE2000 (Page0,Off) (Page0,Off) ()
rxConfig x          = Do (setReg 0x0C x)

-- allow this in on & off?
txConfig           :: Word8 -> NE2000 (Page0,state) (Page0,state) ()
txConfig x          = Do (setReg 0x0D x)

dataConfig         :: Word8 -> NE2000 (Page0,Off) (Page0,Off) ()
dataConfig x        = Do (setReg 0x0E x)

interruptMask      :: Word8 -> NE2000 (Page0,state) (Page0,state) ()
interruptMask x     = Do (setReg 0x0F x)



physAddr           :: Eth.Addr -> NE2000 (Page1,Off) (Page1,Off) ()
physAddr (Eth.Addr d1 d2 d3 d4 d5 d6)
                    = Do (do setReg 0x01 d1 
                             setReg 0x02 d2 
                             setReg 0x03 d3 
                             setReg 0x04 d4 
                             setReg 0x05 d5 
                             setReg 0x06 d6)

setCurrent         :: Word8 -> NE2000 (Page1,state) (Page1,state) ()
setCurrent x        = Do (setReg 0x07 x)

getCurrent         :: NE2000 (Page1,state) (Page1,state) Word8
getCurrent          = Do (getReg 0x07)

-- XXX: No need for list?
multicast          :: [Word8] {-8-} -> NE2000 (Page1,Off) (Page1,Off) ()
multicast bs        = Do (zipWithM_ setReg [0x08 .. 0x0F] bs)


