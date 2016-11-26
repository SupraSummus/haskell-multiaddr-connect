module Network.Multiaddr.Connector.Type (Connection, Connector) where

import Codec.Text.Multipath (Multipath)
import Data.ByteString (ByteString)

type Connection m = m (Maybe ByteString) -> (ByteString -> m ()) -> m ()
type Connector m = Multipath -> Maybe (Multipath, Connection m)
