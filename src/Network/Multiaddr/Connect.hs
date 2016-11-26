module Network.Multiaddr.Connect (connect) where

import Network.Multiaddr.Connector.Type
import Data.Maybe (catMaybes)
import qualified Network.Multiaddr.Connector.IP4.TCP
import qualified Network.Multiaddr.Connector.Loopback

connectors :: [Connector IO]
connectors = [
        Network.Multiaddr.Connector.Loopback.connect,
        Network.Multiaddr.Connector.IP4.TCP.connect
    ]

connect :: Connector IO
connect addr
    | length okConnectors == 0 = Nothing
    | otherwise = Just $ head okConnectors
    where
        okConnectors = catMaybes $ map ($ addr) connectors
