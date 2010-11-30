-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.Network.NE2000.Main(initPCI,initialize) where

import Hypervisor.IOPorts(Port)

import Device.Network.NE2000.Monad hiding (readChan,writeChan,newChan,putMVar)
import qualified Device.Network.NE2000.Monad as NE
import Device.Bits
import Device.Utils

import Device.PCI.Device
import Device.PCI.ConfigSpace
import Device.PCI.BaseAddr

import qualified Device.Network.API.Ethernet as Eth
import qualified Device.Network.API.Interface as Net
import Device.Network.API.Packet
import Device.Network.API.PacketParsing(doParse)

import Control.Concurrent as IO
import Data.Maybe(fromMaybe)




data Event          = CardEvent 
		    | Rx -- check buffer for more packets
                    | Tx (Eth.Packet OutPacket)

instance Show Event where
    show CardEvent = "CardEvent"
    show (Tx _)    = "Tx"
    show Rx        = "Rx"


initPCI            :: (String -> IO())              -- ^ For debug.
                   -> Dev ()
                   -> IO (Maybe (Eth.Interface IO (Eth.Packet InPacket)
                                                  (Eth.Packet OutPacket)))
initPCI dbg dev     
  = do _base <- dev `getBAR` 0 
       prt ("base is "++show _base)
       case _base of
         Mem _ -> return Nothing
         IO base -> 
           do irq <- config dev `get8` 0x3C
              prt ("IRQ is " ++ show irq)
              eth <- initialize prt (toEnum (fromIntegral irq))
                                    (fromIntegral (addr base))
              return (Just eth)
  where
    prt = dbg . ("NE2000: "++)

initialize         :: (String -> IO())              -- ^ For debug.
                   -> IRQ                           -- ^ Interrupt for events.
                   -> Port                          -- ^ Base IO port.
                   -> IO (Eth.Interface IO (Eth.Packet InPacket)
				           (Eth.Packet OutPacket))
initialize prt irq port 
    = do events <- newChan
	 irqAck <- newEmptyMVar 
	 let -- The interrupt handler should not return until it has told
             -- the device to deassert its interrupt signal.
             irqHandler = do writeChan events CardEvent
			     takeMVar irqAck

         -- XXX: Check for error
         set_interrupt irq False irqHandler

         macV   <- newEmptyMVar
         rxChan <- newChan
         forkIO (neRun port prt (card macV rxChan events irqAck))
         mac    <- takeMVar macV 
        
         let io = Net.Interface { Net.rx = IO.readChan rxChan
		                , Net.tx = IO.writeChan events . Tx }
	 return Eth.Interface { Eth.myMAC = mac, Eth.io=io }


card :: MVar Eth.Addr -> Chan (Eth.Packet InPacket) -> Chan Event ->
        MVar ()
        -> NE2000 (page, start) s3 b
card macV rx events irqAck =
  initialize_NE2000 `neBind` NE.putMVar macV `neThen`
  NE.newChan        `neBind` \buffer ->

  let loop          = (NE.readChan events `neBind` handle) `neThen` loop 
{-
      status = getEvents `neBind` \ events ->
	       getCmd `neBind` \ cmd ->
	       debug (showBin cmd++" "++showBin events)
                                       
      handle' ev = debug (show ev) `neThen` status `neThen` handle ev
--}
      --handle' ev = debug "." `neThen` handle ev

      handle (Tx p) = txing `neBind` \busy ->
                      if busy then NE.writeChan buffer p
                              else doTxPacket p

      handle Rx         = rxPackets
      handle CardEvent  = --debug "*" `neThen`
                          turnToPage Page0 `neThen`
			  getEvents `neBind` \isr -> 
			  let rx' = isr `testBit` 0
			      tx = isr `testBit` 1
                          in
			  -- Acknowledge interrupts early
			  neWhen rx' (ackEvent 0) `neThen`
			  neWhen tx (ackEvent 1) `neThen`
			  NE.putMVar irqAck () `neThen`
                          neWhen rx' rxPackets `neThen`
                          neWhen tx txPackets



      -- | Receive up to 10 packets from the card's buffer.
      -- Note: 10 is arb.
      rxPackets     = --ackEvent 0 `neThen`
                      getBoundary `neBind` loop2 (10::Int)
        where 
        loop2 0 _    = NE.writeChan events Rx   -- to resume later
        loop2 n b    = neWhenM (havePacket b) (
                             doRxPacket b `neBind` \(p,b') ->
                             NE.writeChan rx p `neThen`
                             loop2 (n-1) b'
                          )

      txPackets     =  --ackEvent 1 `neThen`
                       neWhenM (not `neMap` NE.emptyChan buffer) (
                        NE.readChan buffer `neBind` doTxPacket 
                      )

  in loop
  where
  -- | Check if there seems to be a packet in the card's buffer.
  -- Having to turn pages here seems very silly.
  havePacket bdry   = turnToPage Page1  `neThen`
                      getCurrent        `neBind` \c ->
                      turnToPage Page0  `neThen`
                      neReturn (not (bdry == c))



-- | Receive a packet.  As argument we get the boundary of the packet.
-- PRE: We have a packet, i.e. boundary is not current
doRxPacket         :: Word8 ->                          -- initial boundary
                        NE2000 (Page0,On) (Page0,On) 
                                 ( Eth.Packet InPacket  -- a packet
                                 , Word8                -- the new boundary
                                 )
doRxPacket bdry     = --debug "?" `neThen`
                      packetHeader bdry `neBind` \hdr -> 
                      let base = bdry `nextTo` (4::Word8) 
                          next = nextBuffer hdr in
                      readMem base (byteCount hdr) `neBind` \bytes ->
                      setBoundary next `neThen`
                      neReturn (fromMaybe err (doParse bytes), next)
  where err = error "NE2000.Driver error: short ethernet packet recevied"

-- | Transmit a packet.
-- PRE: Card is not already transmitting a packet.
-- PRE: The packet is not too short or too long (XXX: Check this here?)
doTxPacket         :: Eth.Packet OutPacket -> NE2000 (Page0,On) (Page0,On) ()
doTxPacket p        = --debug "!" `neThen` 
                      let pack    = Eth.unparse p
                          addr'   = transmitBuffer `nextTo` (0::Word8)
                      in writeMem addr' pack        `neThen`
                         txByteCount (max 64 (outLen pack)) `neThen`
                         txStart

-- | Write a packet to the NIC's buffer.
writeMem           :: Word16 -> OutPacket -> NE2000 (Page0,On) (Page0,On) ()
writeMem a pack     = remoteAddress a                               `neThen`
                      remoteByteCount (fromIntegral (outLen pack))  `neThen`
                      remoteWrite                                   `neThen`
                      neMapM_ dmaTxArray (chunks pack)              `neThen`
                      waitForDMA

-- | Busy loop while DMA completes. 
-- XXX: We could put a limit on this.
-- XXX: Looping is not too bad as we expect only a few loops?
waitForDMA         :: NE2000 (Page0,s) (Page0,s) ()
waitForDMA          = getEvents `neBind` \isr ->
                      neWhen (not (isr `testBit` 6)) waitForDMA `neThen`
                      ackEvent 6

-- | Read a packet from the NIC's buffer.
readMem            :: Word16 -> Int -> NE2000 (Page0,On) (Page0,On) InPacket
readMem a l         = remoteAddress a                     `neThen`
                      remoteByteCount l                   `neThen`
                      remoteRead                          `neThen`
                      dmaRxArray (fromIntegral l)         `neBind` \bytes ->
                      waitForDMA                          `neThen` 
                      -- Prolly not neccessary, do the ack though.
                      neReturn (toInPack bytes)

data NE2000Header   = NE_Hdr
                    { rxStatus    :: !Word8
                    , nextBuffer  :: !Word8
                    , byteCount   :: !Int
                    }

packetHeader       :: Word8 -> NE2000 (Page0,s) (Page0,s) NE2000Header
packetHeader bdry   = let base = bdry `nextTo` (0::Word8) in
                      remoteAddress base  `neThen`
                      remoteByteCount 4   `neThen`
                      getDMA              `neBind` \w1 ->
                      getDMA              `neBind` \w2 ->
                      neReturn $ NE_Hdr 
                        { rxStatus   = w1 .!. 0
                        , nextBuffer = w1 .!. 1
                        , byteCount  = fromIntegral w2
                        }

nicPROM            :: NE2000 (Page0, state) s3 Eth.Addr
nicPROM             = remoteAddress 0     `neThen`
                      remoteByteCount 32  `neThen`
                      dmaRxArray 32       `neBind` \b -> 
                      neReturn (Eth.Addr (b ! 0) (b ! 2) (b ! 4) (b ! 6) 
                                                            (b ! 8) (b ! 10)) 
                    

transmitBuffer     :: Word8
transmitBuffer      = 0x40

receiveBufferStart :: Word8
receiveBufferStart  = 0x46

receiveBufferStop  :: Word8
receiveBufferStop   = 0x80

-- NOTE: qemu does not implement the filtering of packets.

initialize_NE2000  :: NE2000 (page,start) (Page0,On) Eth.Addr
initialize_NE2000   = 
  stop              `neThen`
  turnToPage Page0  `neThen`

  dataConfig 0x49   `neThen` -- little endian, 16 bit data transfers 
  remoteByteCount 0 `neThen`
  rxConfig 0x00     `neThen` -- accept only normal packet 
                             -- (we still get broadcast, why?)
  txConfig 0x02     `neThen` -- internal loop back

  -- receive buffer
  rxBufferStart receiveBufferStart  `neThen`
  rxBufferEnd receiveBufferStop     `neThen`
  setBoundary receiveBufferStart    `neThen`
                        
  -- currently we only use 1 page, so set it here
  -- for 2 page transmit buffer we need to adjust this 

  txPage transmitBuffer   `neThen`
  clearEvents             `neThen`
  interruptMask 0x00      `neThen`
  nicPROM                 `neBind` \a -> 

  turnToPage Page1              `neThen`
  physAddr a                    `neThen`
  multicast (replicate 8 0xFF)  `neThen`  -- accept all 
  setCurrent receiveBufferStart `neThen`  -- put incoming packets here

  turnToPage Page0      `neThen`
  interruptMask 0x03    `neThen` -- we get only receive & transmit events 
  start                 `neThen`
  txConfig 0x00         `neThen` -- normal mode
  neReturn a
