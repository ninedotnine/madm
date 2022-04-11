import Control.Exception qualified as Exception
import Control.Monad (void, foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Functor ((<&>))
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
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Contact
import Settings qualified as Settings
import Database (Database)
import Database qualified as Database
import Message qualified as Message

main :: IO Void
main = do
    aliases_file <- getArgs
                >>= \case
                    [arg] -> pure arg
                    _ -> BS.Char8.putStrLn usage >> exitFailure
                        where
                            usage = "usage: mads-server filename"
    putStrLn $ ["loading ", aliases_file, "..."] & concat
    database <- BS.readFile aliases_file
            <&> Database.parsed
    run_server aliases_file database


run_server :: FilePath -> Database -> IO Void
run_server aliases_file database = do
    addr <- resolve
    void $ Exception.bracket (open addr) close (loop aliases_file database)
    pure $ error "server is terminating."
    where
        resolve :: IO AddrInfo
        resolve = do
            -- FIXME: can getAddrInfo ever return [] ?
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
    (invalids, valids) <- recv sock Settings.max_bytes
                      <&> Message.parse_all
--     (invalids, valids) <- recv sock Settings.max_bytes
--                       <&> BS.split Word8._comma
--                       <&> squashed_commas
--                       <&> map BS.Char8.strip
--                       <&> map parsed
--                       <&> partitionEithers
    mapM_ report invalids
    foldM (Database.update aliases_file) database valids

report :: ByteString -> IO ()
report msg = BS.Char8.putStrLn $ "invalid: " <> msg

