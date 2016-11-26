module Network.Multiaddr.Connector.IP4.TCP (
        Network.Multiaddr.Connector.IP4.TCP.connect
    ) where

import Network.Multiaddr.Connector.Type
import Control.Monad (liftM, when)
import Data.ByteString (ByteString, length)
import Data.Maybe (listToMaybe)
import Network.Socket (
        Socket,
        Family (AF_INET),
        SocketType (Stream),
        SockAddr (SockAddrInet),
        ShutdownCmd (ShutdownSend),
        socket, shutdown, defaultProtocol, connect,
        inet_addr
    )
import Network.Socket.ByteString (send, recv)
import Control.Concurrent (forkIO)

connect :: Connector IO
connect multipath
    | Prelude.length multipath >= 4 = connect4 multipath
    | otherwise = Nothing

connect4 :: Connector IO
connect4 (ipproto:addr:proto:port:encapsulated)
    | (ipproto == "ip4") && (proto == "tcp") = Just (encapsulated, connectTCP addr port)
    | otherwise = Nothing

connectTCP :: String -> String -> Connection IO
connectTCP addrStr portStr dataOut handler = do
    addr <- inet_addr addrStr
    port <- maybe (error $ "invalid port " ++ portStr) (return . fst) (listToMaybe $ reads portStr)
    sock <- socket AF_INET Stream defaultProtocol
    Network.Socket.connect sock $ SockAddrInet port addr
    communicate sock dataOut handler

communicate :: Socket -> Connection IO
communicate sock dataSource recvHandler = do
    forkIO $ sendManyIO sock dataSource
    recvMany sock recvHandler

recvMany :: Socket -> (ByteString -> IO ()) -> IO ()
recvMany sock handler = do
    d <- recv sock 1024
    when (Data.ByteString.length d > 0) $ do
        handler d
        recvMany sock handler

sendManyIO :: Socket -> IO (Maybe ByteString) -> IO ()
sendManyIO sock dataSource = do
    d <- dataSource
    maybe
        (shutdown sock ShutdownSend)
        (\d -> do
            send sock d
            sendManyIO sock dataSource)
        d
