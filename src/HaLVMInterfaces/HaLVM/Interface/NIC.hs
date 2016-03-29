-- |The module describes the generic interface to a network card provided by
-- HaLVM-compatible drivers. In some cases, this may be hidden by a network
-- stack. (In fact, it's probably a better idea to use a higher level
-- interface.)
--
-- General use of this module will be to find the network card you're looking
-- for via the `NetworkCardDriver`, and use that to get a NIC that you can do
-- interesting things with.
--
module HaLVM.Interface.NIC(
         NetworkCardDriver(..)
       , NIC(..)
       , NICOptions(..)
       , NICStatistics(..)
       )
 where

import Control.Concurrent.BoundedChan
import Data.ByteString.Lazy(ByteString)
import Data.IORef(IORef)
import Data.Word(Word64,Word8)

-- |A HaLVM-compatible network card driver.
data NetworkCardDriver = NetworkCardDriver {
       ndriverListNICs      :: IO [String]
     , ndriverInitializeNIC :: String -> NICOptions -> IO (Maybe NIC)
     }

instance Monoid NetworkCardDriver where
  mempty = NetworkCardDriver { ndriverListNICs      = return []
                             , ndriverInitializeNIC = \ _ _ -> return Nothing
                             }
  mappend a b = NetworkCardDriver {
                  ndriverListNICs =
                    do adisks <- ndriverListNICs a
                       bdisks <- ndriverListNICs b
                       return (adisks ++ bdisks)
                , ndriverInitializeNIC =
                    \ x opt ->
                       do mnic <- ndriverInitializeNIC a x opt
                          case mnic of
                            Nothing -> ndriverInitializeNIC b x opt
                            Just _  -> return mnic
               }

-- |A set of options to pass to initialize the network card with. Note that
-- some drivers may not support all of the options listed here. Thus, please
-- check the flags associated with the NIC provided before you assume that
-- your request had an effect. For example, if you set
-- `nicOptTxIPv4ChecksumOffload`, then you should make sure you check
-- `nicTxIPv4ChecksumOffload` before you assume that you don't need to do your
-- own checksums.
data NICOptions = NICOptions {
       nicMAC                   :: Maybe (Word8,Word8,Word8,Word8,Word8,Word8)
     , nicRxIPv4ChecksumOffload :: Bool
     , nicTxIPv4ChecksumOffload :: Bool
     , nicRxIPv6ChecksumOffload :: Bool
     , nicTxIPv6ChecksumOffload :: Bool
     }

-- |A set of statistics capture by the underlying NIC card. Can be useful for
-- understanding the behavior of the system.
data NICStatistics = NICStatistics {
       nstatsBytes   :: IORef Word64 -- ^ # of bytes sent/received
     , nstatsPackets :: IORef Word64 -- ^ # of packets sent/received
     , nstatsDropped :: IORef Word64 -- ^ # of packets dropped
     , nstatsErrors  :: IORef Word64 -- ^ # of packets that caused errors
     }

-- |A HaLVM-compatible NIC.
data NIC = NIC {
       nicName                  :: String
     , nicOptions               :: NICOptions
     , nicTxStatistics          :: NICStatistics
     , nicRxStatistics          :: NICStatistics
     , -- |Start the network card running, using the given BoundedChans as its
       -- input and output queues, respectively. Calling this function twice,
       -- with no intervening `nicStop`, will have unknown consequences that
       -- vary by driver. Don't try it.
       nicStart                 :: BoundedChan ByteString {- ^Input  -} ->
                                   BoundedChan ByteString {- ^Output -} ->
                                   IO ()
     , -- |Stop the network card from operation. This action will safely close
       -- down the card and discard any state. If there are packets in either
       -- queue, they will be ignored without being sent or processed. Calling
       -- `nicStart` is safe, and will resume processing.
       nicStop                  :: IO ()
     }

