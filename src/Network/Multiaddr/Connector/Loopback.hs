module Network.Multiaddr.Connector.Loopback (connect) where

import Network.Multiaddr.Connector.Type
import Data.Maybe (maybe)

connect :: Connector
connect multipath
    | length multipath >= 1 = connect1 multipath
    | otherwise = Nothing

connect1 :: Connector
connect1 (loopback:encapsulated)
    | (loopback == "loopback") = Just (encapsulated, loopbackConn)
    | otherwise = Nothing

loopbackConn :: Connection
loopbackConn dataSource handler = do
    d <- dataSource
    maybe
        (return ())
        (\d -> do
            handler d
            loopbackConn dataSource handler)
        d
