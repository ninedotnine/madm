module MADM.Server.Server (
    run
) where

import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Functor (void, (<&>))
import Data.Function ((&))
import Data.Void (Void)
import Network.Socket (
    accept,
    addrAddress,
    AddrInfo,
    addrFamily,
    addrFlags,
    addrProtocol,
    addrSocketType,
    AddrInfoFlag(AI_PASSIVE),
    bind,
    close,
    defaultHints,
    getAddrInfo,
    listen,
    setCloseOnExecIfNeeded,
    setSocketOption,
    Socket,
    socket,
    SocketOption(ReuseAddr),
    SocketType(Stream),
    withFdSocket,
    )
import Network.Socket.ByteString (recv)

import MADM.Server.Database (Database)
import MADM.Server.Database qualified as Database
import MADM.Server.Message qualified as Message
import Settings qualified

run :: FilePath -> Database -> IO Void
run aliases_file database = do
    addr <- resolve
    void $ Exception.bracket (open addr) close (loop aliases_file database)
    pure $ error "server is terminating."
    where
        resolve :: IO AddrInfo
        resolve = do
            -- getAddrInfo promises to never return []
            head <$> getAddrInfo
                        (Just hints)
                        Nothing
                        (Just Settings.port_number)
                where
                    hints = defaultHints {
                        addrFlags = [AI_PASSIVE],
                        addrSocketType = Stream
                    }
        open :: AddrInfo -> IO Socket
        open addr = do
            sock <- socket
                        (addrFamily addr)
                        (addrSocketType addr)
                        (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            setCloseOnExecIfNeeded & withFdSocket sock
            addr & addrAddress & (sock & bind)
            listen sock 5
            pure sock


loop :: FilePath -> Database -> Socket -> IO Database
loop aliases_file database sock = do
    (conn, _peer) <- accept sock
    new_db <- process_msg aliases_file database conn
    loop aliases_file new_db sock


process_msg :: FilePath -> Database -> Socket -> IO Database
process_msg aliases_file database sock = do
    (invalids, valids) <- recv sock Settings.max_msg_length
                      <&> Message.parsed
    mapM_ report invalids
    Database.update aliases_file database valids


report :: ByteString -> IO ()
report msg = BS.Char8.putStrLn $ "invalid: " <> msg
