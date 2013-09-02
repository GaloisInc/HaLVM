module Common where

import Communication.IVC
import Communication.Rendezvous
import Hypervisor.XenStore

runServer :: XenStore -> (InChannel Int -> IO ()) -> IO ()
runClient :: XenStore -> IO (OutChannel Int)
(runServer, runClient) = clientServerConnection "ClientServerTest" 2
