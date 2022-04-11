import Prelude (IO, Applicative(..))
import Control.Exception qualified as Exception
import Data.Bool (not)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Eq
import Data.Function
import Data.Functor ((<&>))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
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
import System.Exit (exitFailure)

import Settings qualified

main :: IO ()
main = do
    input <- BS.getContents
    BS.putStr input
    case names input of
        "" -> do
            exitFailure
        text -> do
            send_msg "127.0.0.1" Settings.port_number text


send_msg :: HostName -> ServiceName -> ByteString -> IO ()
send_msg host port msg = do
    addr <- resolve
    Exception.bracket (open addr) close (`sendAll` msg)
    where
        resolve = do
            -- is it possible for getAddrInfo to return [] ?
            getAddrInfo (Just hints) (Just host) (Just port) <&> List.head
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
        & List.map BS.Char8.strip
        & List.filter (/= BS.empty)
        & BS.intercalate ","

-- for long "From: " headers spanning multiple lines,
-- all lines after the first are tab-indented.
grep :: Header -> ByteString -> ByteString
grep hdr = BS.Char8.lines
       <&> List.dropWhile (not . BS.isPrefixOf header)
       <&> \case
        [] -> ""
        first:more -> more
                    & List.takeWhile (BS.isPrefixOf "\t")
                    & List.map BS.Char8.strip
                    & BS.Char8.concat
                    & (BS.drop len first <>)
    where
        len = BS.length header
        header = header_string hdr
