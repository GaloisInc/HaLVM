-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.PCI.Device where

import Device.PCI.ConfigSpace

import Numeric(showHex)
import Data.Bits
import Data.ByteString.Char8(ByteString, unpack)
import Data.Word
import Data.List(find)
import Control.Monad(mplus,msum)

type VendorTable    = Word16 -> Maybe (ByteString, DeviceTable)
type DeviceTable    = Word16 -> Maybe (ByteString, SubSysTable)
type SubSysTable    = (Word16,Word16) -> Maybe ByteString
type ClassTable     = Word8 -> Maybe (ByteString, SubClassTable)
type SubClassTable  = Word8 -> Maybe (ByteString, ProgIFTable)
type ProgIFTable    = Word8 -> Maybe ByteString

-- PCI Device
--------------------------------------------------------------------------------
data Dev t          = Dev 
                    { config  :: ConfigSpace
                    , devInfo :: DevInfo
                    , devData :: t
                    }

instance Show (Dev t) where
  show d            = show (config d) ++ " " ++ show (devInfo d)

instance Functor Dev where
  fmap f d          = d { devData = f (devData d) }

showDev :: (VendorTable, ClassTable) -> Dev t -> String
showDev t d         = show (config d) ++ ": " ++ showDevInfo t (devInfo d)

--------------------------------------------------------------------------------


-- Information about a PCI device
--------------------------------------------------------------------------------
data DevInfo        = D { dev_ven   :: !Word32
                        , class_rev :: !Word32
                        , header    :: !Word8
                        }

getDevInfo         :: ConfigSpace -> IO DevInfo
getDevInfo c        = do dv  <- c `get32` 0x00
                         cr  <- c `get32` 0x08
                         hdr <- c `get8`  0x0E
                         return $ D dv cr hdr

vendorId           :: DevInfo -> Word16
vendorId d          = fromIntegral (dev_ven d .&. 0xFFFF)

deviceId           :: DevInfo -> Word16
deviceId d          = fromIntegral (dev_ven d `shiftR` 16)

revision           :: DevInfo -> Word8
revision d          = fromIntegral (class_rev d .&. 0xFF)

rawClass           :: DevInfo -> Word32 -- 24
rawClass d          = class_rev d `shiftR` 8

headerType         :: DevInfo -> Word8
headerType d        = header d `clearBit` 7

isPresent          :: DevInfo -> Bool
isPresent d         = not (vendorId d == 0 || vendorId d == -1)

isMultiFun         :: DevInfo -> Bool
isMultiFun d        = header d `testBit` 7

isPCItoPCIbridge   :: DevInfo -> Bool
isPCItoPCIbridge d  = headerType d == 1


instance Show DevInfo where
  show d            = showHex (vendorId d) $ showChar ' '
                    $ showHex (deviceId d) $ showChar ' '
                    $ showRev d

showDevInfo        :: (VendorTable,ClassTable) -> DevInfo -> String
showDevInfo (v,_) d = case v (vendorId d) of
                        Nothing -> show d
                        Just (t,devs) -> unpack t ++ " " ++
                          case devs (deviceId d) of
                            Nothing -> showHex (deviceId d) $ " " ++ showRev d
                            Just (t',_) -> unpack t' ++ " " ++ showRev d

showRev          :: DevInfo -> String
showRev d         = "(rev " ++ showHex (revision d) ")"

--------------------------------------------------------------------------------


-- PCI Bridges
--------------------------------------------------------------------------------

data PCIBridge      = P { _primBus     :: Word8
                        , _secBus      :: Word8
                        , _subBus      :: Word8
                        } deriving Show

toPCIBridge        :: Dev () -> IO (Maybe (Dev PCIBridge))
toPCIBridge d
  | not (isPCItoPCIbridge (devInfo d)) = return Nothing
  | otherwise       = do x <- config d `get32` 0x18
                         return $ Just
                                $ d { devData = P
                                    { _primBus = fromIntegral x
                                    , _secBus  = fromIntegral (x `shiftR` 8)
                                    , _subBus  = fromIntegral (x `shiftR` 16)
                                    }}

primBus            :: Dev PCIBridge -> Word8
primBus d           = _primBus (devData d)

secBus             :: Dev PCIBridge -> Word8
secBus d            = _secBus (devData d)

subBus             :: Dev PCIBridge -> Word8
subBus d            = _subBus (devData d)
--------------------------------------------------------------------------------



-- Device Tress
--------------------------------------------------------------------------------

data DevTree        = DevTree
                        { busNumber :: Word8
                        , devices   :: [Dev ()]
                        , bridges   :: [(Dev PCIBridge, DevTree)]
                        } deriving Show

drawTree :: (String -> IO ()) -> (VendorTable,ClassTable) -> DevTree -> IO ()
drawTree pr tbl t   = treeLines 0 t
  where
  treeLines o t'    = do mapM_ (pr . showDev tbl) (devices t')
                         mapM_ (bridgeLines o) (bridges t')

  bridgeLines o (d,t') = do pr (tab o (showDev tbl d))
                            treeLines (o+2) t'

tab                :: Int -> String -> String
tab o xs            = replicate o ' ' ++ xs


-- NOTE: Does not consider bridges
findDev            :: Word16 -> Word16 -> DevTree -> Maybe (Dev ())
findDev ven dev t   = find isMe (devices t)
              `mplus` msum (map (findDev ven dev . snd) (bridges t))
  where
  isMe   :: Dev t -> Bool
  isMe d  = vendorId (devInfo d) == ven && deviceId (devInfo d) == dev
--------------------------------------------------------------------------------






