module Main where

import Network.Multiaddr.Connector.Type
import Network.Multiaddr.Connect
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hGetLine, hPutStr, hPutStrLn, stdin, stdout, stderr)
import Data.ByteString.Char8 (pack, unpack)
import Codec.Text.Multipath (Multipath, fromString)
import System.IO.Error (catchIOError)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [pathString] | Right path <- fromString pathString -> do
            connection <- getConnection path
            connection
                (catchIOError ((Just . pack . (++ "\n")) <$> hGetLine stdin) (const $ return Nothing))
                (hPutStr stdout . unpack)
            exitSuccess
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <multiaddr>"
            exitFailure


getConnection :: Multipath -> IO (Connection)
getConnection addr = maybe (ioError $ userError $ "no connector for addr " ++ show addr) (return . snd) (connect addr)
