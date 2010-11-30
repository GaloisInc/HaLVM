{-# LANGUAGE ScopedTypeVariables,ForeignFunctionInterface #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
module XenDevice.PCI(
         dPCI
       , PCIDevice
       , DeviceID(..), VendorID(..)
       , findDevice
       , findDeviceNB
       --
       , pciDeviceID
       , pciVendorID
       , pciCommand
       , setPCICommand
       , pciStatus
       , pciRevision
       , pciClassCode
       , pciCacheLineSize
       , setPCICacheLineSize
       , pciLatencyTimer
       , setPCILatencyTimer
       , pciHeaderType
       , pciBIST
       , pciBAR
       , setPCIBAR
       , pciCardbusCISPointer
       , pciSubsystemVendorID
       , pciSubsystemID
       , pciExpansionROMBaseAddress
       , pciCapPointer
       , pciInterruptLine
       , pciInterruptPin
       , pciMinGnt
       , pciMaxLatency
       , pciReadCapability
       , pciWriteCapability
       )
 where

import Control.Applicative((<$>))
--import Control.Arrow(right)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Numeric
import System.IO.Unsafe(unsafePerformIO)
import System.Timeout

import Hypervisor.Basics
import Hypervisor.Kernel
import Hypervisor.Memory
import Hypervisor.Port
import XenDevice.Xenbus
import XenDevice.XenRingState

import Numeric

#include <xen/io/pciif.h>

data PCIDevice = PCIDevice {
    pciDomain :: Word32
  , pciBus    :: Word32
  , pciDevFn  :: Word32
  , pciPage   :: MVar (Ptr PCIOp) -- You must hold this lock to do ops
  , pciEChan  :: Port
  }

knownDevices :: MVar [(VendorID, DeviceID, PCIDevice)]
knownDevices  = unsafePerformIO $ newMVar []

dPCI :: DeviceDriver
dPCI = DeviceDriver {
    devName      = "XEN PCIFront Driver"
  , dependencies = [dXenbus]
  , initialize   = initializePCIFront
  , shutdown     = return () -- FIXME
  }

-- |Search for the given Vendor/Device pair amongst the known devices. This
-- may block temporarily due to a lock, but will not poll waiting for the
-- device to exist.
findDeviceNB :: VendorID -> DeviceID -> Xen (Maybe PCIDevice)
findDeviceNB v d = do
  devs <- readMVar knownDevices
  case find p devs of
    Just (_, _, x) -> return (Just x)
    Nothing        -> return Nothing
 where p (v', d', _)         = (v == v') && (d == d')

-- |Search for the given Vendor/Device pair, blocking until the device is
-- found. In one way, this is the safer to use variant of findDeviceNB,
-- because there can be arbitrary delay between the kernel booting and
-- the PCI bus recognizing the device exists.
findDevice :: VendorID -> DeviceID -> Xen PCIDevice
findDevice v d = do
  mdev <- findDeviceNB v d
  case mdev of
    Just x  -> return x
    Nothing -> threadDelay (500 * 1000) >> findDevice v d

-- --------------------------------------------------------------------------
-- Higher-level PCI commands
--
--

newtype DeviceID = DeviceID Word16 deriving (Eq,Show)
newtype VendorID = VendorID Word16 deriving (Eq,Show)

pciDeviceID :: PCIDevice -> Xen DeviceID
pciDeviceID dev = DeviceID <$> readConfig dev 2

pciVendorID :: PCIDevice -> Xen VendorID
pciVendorID dev = VendorID <$> readConfig dev 0

pciCommand :: PCIDevice -> Xen Word16
pciCommand dev = readConfig dev 4

setPCICommand :: PCIDevice -> Word16 -> Xen ()
setPCICommand dev = writeConfig dev 4

pciStatus :: PCIDevice -> Xen Word16
pciStatus dev = readConfig dev 6

pciRevision :: PCIDevice -> Xen Word8
pciRevision dev = readConfig dev 8

pciClassCode :: PCIDevice -> Xen Word32
pciClassCode dev = (`shiftR` 8) <$> readConfig dev 8

pciCacheLineSize :: PCIDevice -> Xen Word8
pciCacheLineSize dev = readConfig dev 0x0c

setPCICacheLineSize :: PCIDevice -> Word8 -> Xen ()
setPCICacheLineSize dev = writeConfig dev 0x0c

pciLatencyTimer :: PCIDevice -> Xen Word8
pciLatencyTimer dev = readConfig dev 0x0d

setPCILatencyTimer :: PCIDevice -> Word8 -> Xen ()
setPCILatencyTimer dev = writeConfig dev 0x0d

pciHeaderType :: PCIDevice -> Xen Word8
pciHeaderType dev = readConfig dev 0x0e

pciBIST :: PCIDevice -> Xen Word8
pciBIST dev = readConfig dev 0x0f

pciBAR :: Integral a => PCIDevice -> a -> Xen Word32
pciBAR dev i
  | i >= 0 && i < 5 = readConfig dev (0x10 + (4 * fromIntegral i))
  | otherwise       = xThrow EINVAL

setPCIBAR :: Integral a => PCIDevice -> a -> Word32 -> Xen ()
setPCIBAR dev i v
  | i >= 0 && i < 5 = writeConfig dev (0x10 + (4 * fromIntegral i)) v
  | otherwise       = xThrow EINVAL

pciCardbusCISPointer :: PCIDevice -> Xen Word32
pciCardbusCISPointer dev = readConfig dev 0x28

pciSubsystemVendorID :: PCIDevice -> Xen Word16
pciSubsystemVendorID dev = readConfig dev 0x2c

pciSubsystemID :: PCIDevice -> Xen Word16
pciSubsystemID dev = readConfig dev 0x2e

pciExpansionROMBaseAddress :: PCIDevice -> Xen Word32
pciExpansionROMBaseAddress dev = readConfig dev 0x30

pciCapPointer :: PCIDevice -> Xen Word8
pciCapPointer dev = readConfig dev 0x34 :: Xen Word8

pciInterruptLine :: PCIDevice -> Xen Word8
pciInterruptLine dev = readConfig dev 0x3c

pciInterruptPin :: PCIDevice -> Xen Word8
pciInterruptPin dev = readConfig dev 0x3d

pciMinGnt :: PCIDevice -> Xen Word8
pciMinGnt dev = readConfig dev 0x3e

pciMaxLatency :: PCIDevice -> Xen Word8
pciMaxLatency dev = readConfig dev 0x3f

pciReadCapability :: (Integral a, IsConfigRW b) =>
                     PCIDevice -> a -> Xen b
pciReadCapability dev off
  | (off >= 64) && (off < 4096) = readConfig dev $ fromIntegral off
  | otherwise                   = xThrow EINVAL

pciWriteCapability :: (Integral a, IsConfigRW b) =>
                      PCIDevice -> a -> b -> Xen ()
pciWriteCapability dev off val
  | (off >= 64) && (off < 4096) = writeConfig dev (fromIntegral off) val
  | otherwise                   = xThrow EINVAL

-- ---------------------------------------------------------------------------

initializePCIFront :: IO ()
initializePCIFront = do
  ret <- xsDirectory "device/pci"
  case ret of
    XBOk pci_devs -> mapM_ initializePCIDevice pci_devs
    _             -> hDEBUG $ "VPCI: Couldn't read PCI device base."

initializePCIDevice :: String -> IO ()
initializePCIDevice _nodeName = do
  _beidStr <- xsRead $ nodeName ++ "/backend-id"
  _beStr   <- xsRead $ nodeName ++ "/backend"
  case (_beidStr, _beStr) of
    (XBOk beidStr, XBOk backend) -> do
      let backendId = DomId $ read beidStr
      page               <- allocPage
      ref@(GrantRef rn)  <- allocRef
      port               <- allocPort backendId
      pageMV             <- newMVar page
      grantAccess ref backendId page True
      clearSharedPage page
      let refStr  = show rn
          portStr = drop 1 $ dropWhile (/= ' ') $ show port
      ret <- xsSetWatch (backend ++ "/state") $ onConnect backend pageMV port
      case ret of
        XBOk _ -> do
          xsWrite (nodeName ++ "/pci-op-ref")    refStr
          xsWrite (nodeName ++ "/event-channel") portStr
          xsWrite (nodeName ++ "/magic")         (#const_str XEN_PCI_MAGIC)
          xsWrite (nodeName ++ "/state")         (show xenRingInitialised)
          return ()
        _      -> do
          hDEBUG $ "VPCI: Could not set backend watch."
    _                          ->
      hDEBUG $ "VPCI: Failed to get XenStore info for " ++ nodeName
 where
  nodeName             = "device/pci/" ++ _nodeName
  buildDevFn d f       = ((d .&. 0x1f) `shiftL` 3) .|. (f .&. 7)
  onConnect backend p e watch path = do
    _sStr <- xsRead path
    case _sStr of
      XBOk _s | read _s == xenRingInitialising     -> return ()
              | read _s == xenRingInitialisingWait -> return ()
              | read _s == xenRingInitialised      -> return ()
              | read _s == xenRingClosing          -> return ()
              | read _s == xenRingClosed           -> return ()
              | read _s == xenRingConnected        -> do
        xsUnsetWatch watch
        _rootsStr <- xsRead (backend ++ "/root_num")
        case _rootsStr of
          XBOk rootsStr -> do
            let roots :: Integer
                roots  = read rootsStr
            forM_ [0..roots-1] $ \ rootNum -> do
              mkey <- xsRead $ backend ++ "/dev-" ++ show rootNum
              case mkey of
                XBOk str -> do
                  let (dom,bdf) = break (== ':')          str
                      (bus,df)  = break (== ':') $ drop 1 bdf
                      (dev,pf)  = break (== '.') $ drop 1 df
                      fun       =                  drop 1 pf
                      devfn     = buildDevFn (readH dev) (readH fun)
                      device   = PCIDevice (readH dom) (readH bus) devfn p e
                  probeDevice device
                _            ->
                  hDEBUG $ "VPCI: Couldn't read root info for device " ++
                           show rootNum
            xsWrite (nodeName ++ "/state") (show xenRingConnected)
            return ()
          _             ->
            hDEBUG $ "VPCI: Unable to read roots!"
      XBOk _s    -> hDEBUG $ "VPCI: Got weird ring state: " ++ show _s
      XBError _s -> hDEBUG $ "VPCI: Weird watch error: " ++ show _s
  --
  readH x = case readHex x of
              (v,""):_ -> v
              _        -> error "Couldn't parse domain, device, fun, or bus!"

probeDevice :: PCIDevice -> IO ()
probeDevice dev = do
  vres <- xTry $ pciVendorID dev
  dres <- xTry $ pciDeviceID dev
  case (vres, dres) of
    (Right v, Right d) -> do
       modifyMVar_ knownDevices ( return . ((v, d, dev):))
    (Left e1, Left e2) ->
      hDEBUG $ "VPCI: Couldn't read vendor/device ID for device at "
            ++ ref ++ ": " ++ show e1 ++ "/" ++ show e2
    (Left e, _       ) ->
      hDEBUG $ "VPCI: Couldn't read vendor ID for device at "
            ++ ref ++ ": " ++ show e
    (_,       Left e)  ->
      hDEBUG $ "VPCI: Couldn't read device ID for device at "
            ++ ref ++ ": " ++ show e
 where
  ref = showHex (pciDomain dev)    "" ++ ":" ++
        showHex (pciBus dev)       "" ++ ":" ++
        showHex (g $ pciDevFn dev) "" ++ "." ++
        showHex (f $ pciDevFn dev) ""
  g x = (x `shiftR` 3) .&. 0x1f
  f x = x .&. 0x7

hDEBUG :: String -> IO ()
hDEBUG = putStrLn

-- --------------------------------------------------------------------------
-- Low-level PCI commands
--
--

class Integral a => IsConfigRW a where
  rwsize      :: a -> Int32

readConfig :: IsConfigRW a => PCIDevice -> Int -> Xen a
readConfig dev off = foo undefined
 where
  foo :: IsConfigRW a => a -> Xen a
  foo x = fromIntegral <$> readBusRaw dev off (rwsize x)

writeConfig :: IsConfigRW a => PCIDevice -> Int -> a -> Xen ()
writeConfig dev off val = writeBusRaw dev off (rwsize val) (fromIntegral val)

instance IsConfigRW Word8  where rwsize _ = 1
instance IsConfigRW Word16 where rwsize _ = 2
instance IsConfigRW Word32 where rwsize _ = 4
instance IsConfigRW Int8   where rwsize _ = 1
instance IsConfigRW Int16  where rwsize _ = 2
instance IsConfigRW Int32  where rwsize _ = 4

writeBusRaw :: PCIDevice -> Int -> Int32 -> Word32 -> Xen ()
writeBusRaw dev offset size val = do
  page <- takeMVar $ pciPage dev -- this must be held until we're done
  mres <- doPCIOp page (pciEChan dev) op
  res  <- case mres of
            Nothing                     -> xThrow EBUSY
            Just res | opError res == 0 -> return  ()
                     | otherwise        ->
              xThrow $ toEnum $ fromIntegral $ -(opError res)
  putMVar (pciPage dev) page
  return res
 where
  op = zeroPCIOp {
         opCommand  = (#const XEN_PCI_OP_conf_write)
       , opDomain   = pciDomain dev
       , opBus      = pciBus dev
       , opFunction = pciDevFn dev
       , opOffset   = fromIntegral offset
       , opSize     = size
       , opValue    = val
       }

readBusRaw :: PCIDevice -> Int -> Int32 -> Xen Word32
readBusRaw dev offset size = do
  page <- takeMVar $ pciPage dev -- this must be held until we're done
  mres <- doPCIOp page (pciEChan dev) op
  res  <- case mres of
            Nothing                     -> xThrow EBUSY -- ?
            Just res | opError res == 0 -> return $ opValue res
                     | otherwise        ->
              xThrow $ toEnum $ fromIntegral $ -(opError res)
  putMVar (pciPage dev) page
  return res
 where
  op = zeroPCIOp {
         opCommand  = (#const XEN_PCI_OP_conf_read)
       , opDomain   = pciDomain dev
       , opBus      = pciBus dev
       , opFunction = pciDevFn dev
       , opOffset   = fromIntegral offset
       , opSize     = size
       }

-- --------------------------------------------------------------------------
--
-- PCI Shared Page
--

doPCIOp :: Ptr a -> Port -> PCIOp -> IO (Maybe PCIOp)
doPCIOp ptr port op = do
  unsetPortHandler port -- they really are out to get me
  mvar <- newEmptyMVar
  setPortHandler port $ do
    stillActive <- checkFrontActiveBit ptr
    unless stillActive $ do
      putMVar mvar =<< peekByteOff ptr (#offset struct xen_pci_sharedinfo,op)
      clearFrontActiveBit ptr
      unsetPortHandler port
      return ()
  pokeByteOff ptr (#offset struct xen_pci_sharedinfo,op) op
  systemWMB
  setFrontActiveBit ptr
  sendOnPort port
  mres <- timeout (3 * 1000000) $ takeMVar mvar
  case mres of
    Just _   -> return mres
    Nothing  -> do
      unsetPortHandler port
      tryTakeMVar mvar


-- According to the header file, the shared page is a 32-bit flags field,
-- followed by a xen_pci_op structure (op), followed by an AER op field
-- (aer_op).
clearSharedPage :: Ptr a -> IO ()
clearSharedPage p = bzero p 4096

{- 

these for completeness, but not needed for this driver

setBackActiveBit :: Ptr a -> IO ()
setBackActiveBit ptr = do
  current :: Word32 <- peek $ castPtr ptr
  poke (castPtr ptr) (current `setBit` (#const _XEN_PCIB_active))

clearBackActiveBit :: Ptr a -> IO ()
clearBackActiveBit ptr = do
  current :: Word32 <- peek $ castPtr ptr
  poke (castPtr ptr) (current `clearBit` (#const _XEN_PCIB_active))

-}

{-

FIXME: AER stuff?

setAERHandlerBit :: Ptr a -> IO ()
setAERHandlerBit ptr = do
  current :: Word32 <- peek $ castPtr ptr
  poke (castPtr ptr) (current `setBit` (#const _XEN_PCIB_AERHANDLER))

clearAERHandlerBit :: Ptr a -> IO ()
clearAERHandlerBit ptr = do
  current :: Word32 <- peek $ castPtr ptr
  poke (castPtr ptr) (current `clearBit` (#const _XEN_PCIB_AERHANDLER))

-}

setFrontActiveBit :: Ptr a -> IO ()
setFrontActiveBit ptr = do
  current <- peek $ castPtr ptr
  poke (castPtr ptr) (current `setBit` (#const _XEN_PCIF_active) :: Word32)

clearFrontActiveBit :: Ptr a -> IO ()
clearFrontActiveBit ptr = do
  current <- peek $ castPtr ptr
  poke (castPtr ptr) (current `clearBit` (#const _XEN_PCIF_active) :: Word32)

checkFrontActiveBit :: Ptr a -> IO Bool
checkFrontActiveBit ptr = do
  current <- peek $ castPtr ptr
  return $ (current :: Word32) `testBit` (#const _XEN_PCIF_active)

-- --------------------------------------------------------------------------
--
-- PCIOp structures and readers/writers
--

zeroPCIOp :: PCIOp
zeroPCIOp = PCIOp 0 0 0 0 0 0 0 0 0 msis
 where msis = replicate (#const SH_INFO_MAX_VEC) (MSIEntry 0 0)

data MSIEntry = MSIEntry {
    msiVector :: Word16
  , msiEntry  :: Word16
  }
 deriving (Show)

instance Storable MSIEntry where
  sizeOf _     = (#size struct xen_msix_entry)
  alignment _  = 0
  peek ptr     = do
    vec <- (#peek struct xen_msix_entry,vector) ptr
    ent <- (#peek struct xen_msix_entry,entry)  ptr
    return $ MSIEntry vec ent
  poke ptr val = do
    (#poke struct xen_msix_entry,vector) ptr $ msiVector val
    (#poke struct xen_msix_entry,entry)  ptr $ msiEntry val

data PCIOp = PCIOp {
    opCommand    :: Word32     -- OUT: what action to perform: XEN_PCI_OP_*
  , opError      :: Int32      -- IN: Will contain a standard error number
  , opDomain     :: Word32     -- OUT: what device to talk to
  , opBus        :: Word32     -- OUT: what device to talk to
  , opFunction   :: Word32     -- OUT: what device to talk to
  , opOffset     :: Int32      -- OUT: config register to touch
  , opSize       :: Int32      -- OUT: Size of th evalue
  , opValue      :: Word32     -- IN: read value / OUT: value to write
  , opInfo       :: Word32     -- OUT: Extra info for the op
  , opMSIEntries :: [MSIEntry] -- OUT: Param for MSI-X
  }
 deriving (Show)

instance Storable PCIOp where
  sizeOf _     = (#size struct xen_pci_op)
  alignment _  = 0
  peek ptr     = do
    let msi_off = (#offset struct xen_pci_op,msix_entries)
    cmd <- (#peek struct xen_pci_op,cmd) ptr
    err <- (#peek struct xen_pci_op,err) ptr
    dom <- (#peek struct xen_pci_op,domain) ptr
    bus <- (#peek struct xen_pci_op,bus) ptr
    dev <- (#peek struct xen_pci_op,devfn) ptr
    off <- (#peek struct xen_pci_op,offset) ptr
    siz <- (#peek struct xen_pci_op,size) ptr
    val <- (#peek struct xen_pci_op,value) ptr
    inf <- (#peek struct xen_pci_op,info) ptr
    msi <- peekArray (#const SH_INFO_MAX_VEC) (ptr `plusPtr` msi_off)
    return $ PCIOp cmd err dom bus dev off siz val inf msi
  poke ptr val = do
    let msi_off = (#offset struct xen_pci_op,msix_entries)
    (#poke struct xen_pci_op,cmd)    ptr $ opCommand  val
    (#poke struct xen_pci_op,err)    ptr $ opError    val
    (#poke struct xen_pci_op,domain) ptr $ opDomain   val
    (#poke struct xen_pci_op,bus)    ptr $ opBus      val
    (#poke struct xen_pci_op,devfn)  ptr $ opFunction val
    (#poke struct xen_pci_op,offset) ptr $ opOffset   val
    (#poke struct xen_pci_op,size)   ptr $ opSize     val
    (#poke struct xen_pci_op,value)  ptr $ opValue    val
    (#poke struct xen_pci_op,info)   ptr $ opInfo     val
    () <- assert (length (opMSIEntries val) == (#const SH_INFO_MAX_VEC)) $ return ()
    pokeArray (ptr `plusPtr` msi_off) $ opMSIEntries val

-- --------------------------------------------------------------------------
--
-- PCIOp structures and readers/writers
--

data PCIEAEROp = PCIEAEROp {
    aerCommand  :: Word32
  , aerError    :: Int32
  , aerDomain   :: Word32
  , aerBus      :: Word32
  , aerFunction :: Word32
  }

instance Storable PCIEAEROp where
  sizeOf    _ = (#size struct xen_pcie_aer_op)
  alignment _ = 0
  peek    ptr = do
    c <- (#peek struct xen_pcie_aer_op,cmd)    ptr
    e <- (#peek struct xen_pcie_aer_op,err)    ptr
    d <- (#peek struct xen_pcie_aer_op,domain) ptr
    b <- (#peek struct xen_pcie_aer_op,bus)    ptr
    f <- (#peek struct xen_pcie_aer_op,devfn)  ptr
    return $ PCIEAEROp c e d b f
  poke ptr  v = do
    (#poke struct xen_pcie_aer_op,cmd)    ptr $ aerCommand  v
    (#poke struct xen_pcie_aer_op,err)    ptr $ aerError    v
    (#poke struct xen_pcie_aer_op,domain) ptr $ aerDomain   v
    (#poke struct xen_pcie_aer_op,bus)    ptr $ aerBus      v
    (#poke struct xen_pcie_aer_op,devfn)  ptr $ aerFunction v

foreign import ccall unsafe "strings.h bzero"
  bzero :: Ptr a -> Word32 -> IO ()
