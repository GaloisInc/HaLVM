-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.IDE.API where

import Hypervisor.Basics(Xen)
import Hypervisor.IOPorts
import Hypervisor.Memory(mptrToPtr, MPtr, VPtr)
import Device.PCI.Device
import Device.PCI.ConfigSpace
import Device.IDE.DMA as DMA
import Device.IDE.Info
import Device.Bits
import Device.Utils

import Control.Monad(when)
import Control.Concurrent(MVar,newEmptyMVar,putMVar)
import Foreign.Ptr
import Data.Array.IO
import Data.Array.Unboxed
import Data.Word


-- Resources for a device running in compatability mode.
-- XXX: should be in PCI.
get_bar :: Dev a -> Word8 -> IO Port
get_bar d a = do w <- config d `get16` (0x10 + 4 * a)
                 return (w .&. complement 0x0003)

data Resources = Resources
  { command_block_regs :: Port
  , control_block_regs :: Port
  , dma_regs           :: Port
  , interrupt          :: Word8
  , dma_done           :: MVar (Maybe Error)
  }

basic_chan :: Dev a -> IO Resources
basic_chan d =
  do dmar <- d `get_bar` 4
     var  <- newEmptyMVar
     return Resources
       { command_block_regs  = 0
       , control_block_regs  = 0
       , interrupt           = 0
       , dma_regs            = dmar
       , dma_done            = var
       }

data Device = Device
  { resources       :: Resources
  , number          :: Word1
  , info            :: Info
  , dma_descriptors :: DMA.Table
  }

compat_prim :: Dev a -> IO Resources
compat_prim d   = do c <- basic_chan d
                     return c
                       { command_block_regs = 0x1F0
                       , control_block_regs = 0x3F6
                       , interrupt          = 14
                       }

compat_second :: Dev a -> IO Resources
compat_second d = do c <- basic_chan d
                     return c
                       { command_block_regs  = 0x170
                       , control_block_regs  = 0x376
                       , dma_regs            = dma_regs c + 8
                       , interrupt           = 15
                       }

-- Note: not tested, note that the interrupt is shared
native_prim :: Dev a -> IO Resources
native_prim d     = do c <- basic_chan d
                       cmd  <- get_bar d 0
                       ctrl <- get_bar d 1
                       irq  <- config d `get8` 0x3C
                       return c
                         { command_block_regs = cmd
                         , control_block_regs = ctrl
                         , interrupt          = irq
                         }

-- Note: not tested
native_second :: Dev a -> IO Resources
native_second d   = do c <- basic_chan d
                       cmd  <- get_bar d 2
                       ctrl <- get_bar d 3
                       irq  <- config d `get8` 0x3C
                       return c
                         { command_block_regs = cmd
                         , control_block_regs = ctrl
                         , dma_regs           = dma_regs c + 8
                         , interrupt          = irq
                         }

-- | Returns the resources for the two channels supported by
-- the IDE controller.
get_resources :: Dev a -> IO (Resources,Resources)
get_resources dev =
  do classrev <- config dev `get32` 0x08
     let base_class = classrev `shiftR` 24
         sub_class  = (classrev `shiftR` 16) .&. 0xFF
     when (base_class /= 1 || sub_class /= 1) $
       ioError (userError "Unexpected base & sub class")

     let prim_native    = classrev `testBit` 8
         second_native  = classrev `testBit` 10
     prim   <- if prim_native then native_prim dev else compat_prim dev
     second <- if second_native then native_second dev else compat_second dev
     return (prim,second)


get_devices :: Dev a -> IO [Device]
get_devices dev = do (prim,second) <- get_resources dev
                     m1 <- check prim 0
                     m2 <- check prim 1
                     install_chan_handler prim

                     m3 <- check second 0
                     m4 <- check second 1
                     install_chan_handler second

                     return (concat [m1,m2,m3,m4])
  where check r n =
          do select_device r n
             i <- parse_device_info `fmap` identify_device r
             if total_sectors i > 0   -- is there a better way to check this?
                then do p <- DMA.alloc_table
                        return [ Device { resources = r
                                        , number    = n
                                        , info      = i
                                        , dma_descriptors = p
                                        }
                               ]
                else return []
           `catch` \_ -> return []



-- Device registers ------------------------------------------------------------
get_data_port            :: Resources -> IO Word16
get_data_port r           = in16  (command_block_regs r + 0)
set_data_port            :: Resources -> Word16 -> IO ()
set_data_port r n         = out16 (command_block_regs r + 0) n
get_error_reg            :: Resources -> IO Word8
get_error_reg r           = in8   (command_block_regs r + 1)
get_sector_count_28      :: Resources -> IO Word8
get_sector_count_28 r     = in8   (command_block_regs r + 2)
set_sector_count_28      :: Resources -> Word8 -> IO ()
set_sector_count_28 r n   = out8  (command_block_regs r + 2) n
{- WRONG
set_sector_count_48 r n   = do out8 (command_block_regs r + 2) (n `byte` 1)
                               out8 (command_block_regs r + 2) (n `byte` 0)
-}

get_lba_low              :: Resources -> IO Word8
get_lba_low r             = in8   (command_block_regs r + 3)
set_lba_low              :: Resources -> Word8 -> IO ()
set_lba_low r n           = out8  (command_block_regs r + 3) n
get_lba_mid              :: Resources -> IO Word8
get_lba_mid r             = in8   (command_block_regs r + 4)
set_lba_mid              :: Resources -> Word8 -> IO ()
set_lba_mid r n           = out8  (command_block_regs r + 4) n
get_lba_high             :: Resources -> IO Word8
get_lba_high r            = in8   (command_block_regs r + 5)
set_lba_high             :: Resources -> Word8 -> IO ()
set_lba_high r n          = out8  (command_block_regs r + 5) n
get_dev                  :: Resources -> IO Word8
get_dev r                 = in8   (command_block_regs r + 6)
set_dev                  :: Resources -> Word8 -> IO ()
set_dev r n               = out8  (command_block_regs r + 6) n
get_status               :: Resources -> IO Word8
get_status r              = in8   (command_block_regs r + 7)
set_command              :: Resources -> Word8 -> IO ()
set_command r n           = out8  (command_block_regs r + 7) n


set_lba_48 :: Resources -> Word1 -> Word64 -> IO ()
set_lba_48 r d l48 =
  do let lower = fromIntegral l48 :: Word32
     let upper = fromIntegral (l48 `shiftR` 32) :: Word32
     set_lba_low  r (lower `byte` 3)
     set_lba_low  r (lower `byte` 0)
     set_lba_mid  r (upper `byte` 0)
     set_lba_mid  r (lower `byte` 1)
     set_lba_high r (upper `byte` 1)
     set_lba_high r (lower `byte` 2)
     set_dev      r (0x4F .|. (fromIntegral d `shiftL` 4))

-- NOTE: assumes that interruts are enabled
-- XXX: does not seem to work?
get_lba_48 :: Resources -> IO (Word1,Word64)
get_lba_48 r =
  do set_control r 0x00
     l0 <- get_lba_low r
     l1 <- get_lba_mid r
     l2 <- get_lba_high r
     set_control r 0x80
     l3 <- get_lba_low r
     u0 <- get_lba_mid  r
     u1 <- get_lba_high r
     set_control r 0x00

     d  <- get_dev r
     return (  fromIntegral (d `shiftR` 4)
            ,  fromIntegral l0 `shiftL` 0
           .|. fromIntegral l1 `shiftL` 8
           .|. fromIntegral l2 `shiftL` 16
           .|. fromIntegral l3 `shiftL` 24
           .|. fromIntegral u0 `shiftL` 32
           .|. fromIntegral u1 `shiftL` 40
            )

set_lba_28 :: Resources -> Word1 -> Word32 -> IO ()
set_lba_28 r d lba28 =
  do set_lba_low r  (lba28 `byte` 0)
     set_lba_mid r  (lba28 `byte` 1)
     set_lba_high r (lba28 `byte` 2)
     set_dev r (0x40 .|. (fromIntegral d `shiftL` 4)
                     .|. ((lba28 `byte` 3) .&. 0x0F))

get_lba_28   :: Resources -> IO (Word1,Word32)
get_lba_28 r = do b0 <- get_lba_low r
                  b1 <- get_lba_mid r
                  b2 <- get_lba_high r
                  b3 <- get_dev r
                  return (  fromIntegral (b3 `shiftR` 4)
                         ,  fromIntegral (b0 `shiftL` 0)
                        .|. fromIntegral (b1 `shiftL` 8)
                        .|. fromIntegral (b2 `shiftL` 16)
                        .|. fromIntegral ((b3 .&. 0x0F) `shiftL` 24)
                         )

get_alt_status       :: Resources -> IO Word8
get_alt_status r      = in8   (control_block_regs r + 2)
set_control          :: Resources -> Word8 -> IO ()
set_control r n       = out8  (control_block_regs r + 2) n

set_dma_command      :: Resources -> Word8 -> IO ()
set_dma_command r n   = out8  (dma_regs r + 0) n
get_dma_status       :: Resources -> IO Word8
get_dma_status r      = in8   (dma_regs r + 1)
set_dma_status       :: Resources -> Word8 -> IO ()
set_dma_status r n    = out8  (dma_regs r + 1) n

-- | Note: for the moment, the physical address has to fit in 32-bits.
-- (If PEA is enabled, the address could be larger, in theory)
set_dma_table        :: Resources -> MPtr a -> IO ()
set_dma_table r m     = do let w = fromIntegral (ptrToWordPtr (mptrToPtr m))
                           out32 (dma_regs r + 4) w

interrupts           :: Resources -> Bool -> IO ()
interrupts r i        = set_control r (if i then 0x00 else 0x02)
--------------------------------------------------------------------------------


-- | Each channel supports up to two devices.
select_device :: Resources -> Word1 -> IO ()
select_device r n = wait (100::Int) >> set_dev r dev
  where
  dev             = fromIntegral n `shiftL` 4
  wait x | x <= 0 = ioError (userError "select device timed out")
  wait x          = do s <- get_status r
                       when (s .&. 0x88 /= 0) (wait (x-1))


-- Assumes that we have already selected a device.
pio :: (s -> IO s) -> Resources -> s -> IO (Word32, Maybe Error)
pio io r buffer = interrupts r False >> loop (100::Int) 0 buffer
  where loop wait _ _ | wait <= 0 = ioError (userError "pio_send: time out")
        loop wait bytes buf = seq bytes $
          do get_alt_status r
             s <- get_status r
             case () of
               _ | s `testBit` 0  -> do err <- get_error r
                                        return (bytes, Just err)
                 | s `testBit` 7  -> loop (wait - 1) bytes buf
                 | s `testBit` 3  -> do buf1 <- io buf
                                        loop 100 (bytes + 2) buf1
                 | otherwise      -> return (bytes, Nothing)

data Error = Error { err_dev    :: Word1
                   , err_sector :: Word64
                   , err_type   :: ErrorType
                   } deriving Show

-- Note: we assume that only one of the status bits
-- can will be set at the same time.
data ErrorType
  = CannotReadWrite
  | BadAddress
  | Aborted
  | MediaChanged
  | MediaChangeRequest
  | NoMedia
  | Unknwon
    deriving (Eq,Show)

-- XXX: For now, read error location as an lba-28 address.
get_error :: Resources -> IO Error
get_error r =
  do w <- get_error_reg r
     (dev,lba) <- get_lba_28 r
     return Error { err_dev = dev
                  , err_sector = fromIntegral lba
                  , err_type = case () of
                      _ | w `testBit` 6 -> CannotReadWrite
                        | w `testBit` 5 -> MediaChanged
                        | w `testBit` 4 -> BadAddress
                        | w `testBit` 3 -> MediaChangeRequest
                        | w `testBit` 2 -> Aborted
                        | w `testBit` 1 -> NoMedia
                        | otherwise     -> Unknwon
                  }


-- Note: Assumes that a device is selected and ready.
identify_device :: Resources -> IO (UArray Word8 Word16)
identify_device r = do arr <- newArray (0,255) 0
                       set_command r 0xEC -- IDENTIFY_DEVICE
                       pio (get_data arr) r 0
                       -- this is safe because we won't update the array.
                       unsafeFreeze arr

  where get_data :: IOUArray Word8 Word16 -> Word8 -> IO Word8
        get_data arr i = do w <- get_data_port r
                            writeArray arr i w
                            return (i+1)

dma :: DMACommand -> Device -> Word32 -> Word8 -> [(Word16,VPtr a)] -> IO ()
dma cmd d sector count buffs =
  do let t = dma_descriptors d
         r = resources d
     fill_table t buffs
     set_dma_table r =<< phys_addr t
     set_lba_28 r (number d) sector
     set_sector_count_28 r count
     set_command r (dma_code cmd)
     interrupts r True
     set_dma_command r 0x01   -- start

install_chan_handler :: Resources -> Xen Bool
install_chan_handler r =
  set_interrupt (fromIntegral (interrupt r)) False $
    do get_alt_status r
       s <- get_status r
       res <- if s `testBit` 0 then Just `fmap` get_error r
                               else return Nothing
       putMVar (dma_done r) res

