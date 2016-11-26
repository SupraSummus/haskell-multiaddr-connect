module Network.Multiaddr.Connector.Loopback (connect) where

import Network.Multiaddr.Connector.Type
import Data.Maybe (maybe)

connect :: Monad m => Connector m
connect multipath
    | length multipath >= 1 = connect1 multipath
    | otherwise = Nothing

connect1 :: Monad m => Connector m
connect1 (loopback:encapsulated)
    | (loopback == "loopback") = Just (encapsulated, loopbackConn)
    | otherwise = Nothing

loopbackConn :: Monad m => Connection m
loopbackConn dataSource handler = do
    d <- dataSource
    maybe
        (return ())
        (\d -> do
            handler d
            loopbackConn dataSource handler)
        d
