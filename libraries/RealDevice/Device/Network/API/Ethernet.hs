-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.Network.API.Ethernet where

-- Ethernet protocol
-- reference: http://www.techfest.com/networking/lan/ethernet.htm
--            http://www.iana.org/assignments/ethernet-numbers

import Device.Bits
import Device.Network.API.Packet
import Device.Network.API.PacketParsing as P
import qualified Device.Network.API.Interface as Net
import Device.Network.API.Utils(Container(..))
import Data.List(intersperse)
import qualified Numeric

-- Ethernet card drivers should provide the following interface:
data Interface m i o =
  Interface { myMAC :: Addr,
	      io    :: Net.Interface m i o }

rx :: Interface m i o -> m i
rx = Net.rx . io

tx :: Interface m i o -> o -> m ()
tx = Net.tx . io

data Addr           = Addr !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
                      deriving (Eq,Ord,Bounded)

broadcastAddr :: Addr
broadcastAddr = maxBound 
--zeroAddr = minBound :: Addr

instance Parse Addr where
  parse = Addr # parse <# parse <# parse <# parse <# parse <# parse

instance Unparse Addr where
  unparse (Addr b1 b2 b3 b4 b5 b6) = P.unparse ((b1,b2,b3),(b4,b5,b6))

instance Show Addr where
  show (Addr x1 x2 x3 x4 x5 x6)  =
      concat $ intersperse ":" $ map (showHex' 2) [x1,x2,x3,x4,x5,x6]
   where
    showHex x = Numeric.showHex x []
    showHex' n = reverse . take n . (++repeat '0') . reverse . showHex

-- We omit the preambe, startframe delimeter & frame check sequence as
-- we assume that they will be dealt with by the hardware.
data Packet content = Packet
                    { dest      :: Addr
                    , source    :: Addr
                    , packType  :: PacketType
                    , content   :: content
                    }
                    deriving Show

instance Functor   Packet where fmap f p = p { content = f (content p) }
instance Container Packet where contents = content

data PacketType     = Ethernet !Int
                    | IPv4
                    | IPv6
                    | ARP
                    | Unknown !Word16
                    deriving (Eq,Show)

instance Enum PacketType where
  toEnum x
    | x < 0x600     = Ethernet x
  toEnum x          = case x of
                        0x0800  -> IPv4
                        0x86DD  -> IPv6
                        0x0806  -> ARP
                        _       -> Unknown (fromIntegral x)

  fromEnum x        = case x of
                        Ethernet x' -> x'
                        IPv4        -> 0x0800
                        IPv6        -> 0x86DD
                        ARP         -> 0x0806
                        Unknown x'  -> fromIntegral x'
                        
instance Parse PacketType where parse = toEnum . fromIntegral # word16  


instance Parse content => Parse (Packet content) where
  parse = Packet # parse <# parse <# parse <# parse

-- Packets should be paddded elsewhere to satisfy minimum length requirement
-- (46 bytes of data, 64 bytes including headers and CRC)
unparse            :: Packet OutPacket -> OutPacket
unparse p           = addChunk (listArray (0,13) header) (content p)
  where header                 = [ d1, d2, d3, d4, d5, d6
                                , s1, s2, s3, s4, s5, s6
                                , ty .!. 1, ty .!. 0
                                ]
        Addr d1 d2 d3 d4 d5 d6 = dest p
        Addr s1 s2 s3 s4 s5 s6 = source p
        ty                    = fromEnum (packType p) 






