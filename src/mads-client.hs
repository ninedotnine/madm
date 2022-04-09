import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified
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
import System.Exit (exitFailure)

import Settings qualified

main :: IO ()
main = do
    input <- BS.getContents
    case names input of
        "" -> do
            BS.putStr input
            exitFailure
        text -> do
            send_msg "127.0.0.1" Settings.port_number text
            BS.putStr input


send_msg :: HostName -> ServiceName -> ByteString -> IO ()
send_msg host port msg = do
    addr <- resolve
    Exception.bracket (open addr) close (\sock -> sendAll sock msg)
    where
        resolve = do
            -- is it possible for getAddrInfo to return [] ?
            getAddrInfo (Just hints) (Just host) (Just port) <&> head
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

data Header = From | To | CC

header_string :: Header -> ByteString
header_string = \case
    From -> "From: "
    To -> "To: "
    CC -> "CC: "

names :: ByteString -> ByteString
names txt = [
    grep From,
    grep To,
    grep CC
    ] <&> ($ txt)
        & map BS.Char8.strip
        & BS.intercalate ","

-- for long "From: " headers spanning multiple lines,
-- all lines after the first are tab-indented.
grep :: Header -> ByteString -> ByteString
grep hdr txt = txt
    & BS.Char8.lines
    & Data.List.dropWhile (not . BS.isPrefixOf header)
    & \case
        [] -> ""
        first:more -> more
                    & Data.List.takeWhile (BS.isPrefixOf "\t")
                    & map BS.Char8.strip
                    & BS.Char8.concat
                    & (BS.drop len first <>)
    where
        len = BS.length header
        header = header_string hdr
