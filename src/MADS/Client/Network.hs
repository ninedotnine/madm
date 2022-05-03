module MADS.Client.Network (
    send_msg,
) where

import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Network.Socket (
    addrSocketType,
    addrAddress,
    addrFamily,
    addrProtocol,
    connect,
    close,
    defaultHints,
    getAddrInfo,
    HostName,
    ServiceName,
    socket,
    SocketType(Stream),
    )
import Network.Socket.ByteString (sendAll)


send_msg :: HostName -> ServiceName -> ByteString -> IO ()
send_msg host port msg = do
    addr <- resolve
    Exception.bracket (open addr) close (`sendAll` msg)
    where
        resolve = do
            -- getAddrInfo promises to never return []
            List.head <$> getAddrInfo
                            (Just hints)
                            (Just host)
                            (Just port)
                where
                    hints = defaultHints {
                        addrSocketType = Stream
                    }
        open addr = do
            sock <- socket
                (addrFamily addr)
                (addrSocketType addr)
                (addrProtocol addr)
            connect sock $ addrAddress addr
            pure sock
