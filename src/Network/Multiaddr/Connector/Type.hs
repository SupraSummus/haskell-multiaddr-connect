module Network.Multiaddr.Connector.Type (Connection, Connector) where

import Codec.Text.Multipath (Multipath)
import Data.ByteString (ByteString)

type Connection = IO (Maybe ByteString) -> (ByteString -> IO ()) -> IO ()
type Connector = Multipath -> Maybe (Multipath, Connection)
