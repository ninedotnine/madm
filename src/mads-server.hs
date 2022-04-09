import Control.Exception qualified as Exception
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Char (toLower, isSpace)
import Data.Void (Void)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (isInfixOf)
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

import Text.Read (readMaybe)

import Common
import Settings

max_bytes :: Int
max_bytes = 1023

type Database = ([Address] , [Address])

main :: IO Void
main = do
    putStrLn "starting server..."
    database <- ByteString.readFile alias_file <&> parse_database
    run_server database


parse_database :: ByteString -> Database
-- parse_database = lines <&> filter (not . all isSpace) <&> map addr <&> (,[])
parse_database = ByteString.Char8.lines <&> filter (not . ByteString.Char8.all isSpace) <&> map ByteString.Char8.unpack <&> map addr <&> (,[])
    where
        addr = words <&> last <&> Address <&> normalize


insert :: Database -> Address -> Database
insert (db0, db1) addr = (db0, addr:db1)

is_new_and_valid :: Address -> Database -> Bool
is_new_and_valid addr db = is_valid && is_new
    where
        is_new = not $ db `contains` addr
        is_valid = not $ or $
            (`isInfixOf` (from_address addr)) <$> disallowed
                where
                    disallowed = [
                        "noreply",
                        "no-reply",
                        "donot-reply",
                        "do-not-reply",
                        "nepasrepondre"
                        ]


contains :: Database -> Address -> Bool
contains (db0, db1) addr = n_addr `elem` db0 || n_addr `elem` db1
    where
        n_addr = normalize addr

save :: Alias -> IO ()
save entry = putStrLn ("saving: " <> line) >> appendFile alias_file line
    where
        line = "alias " ++ showAlias entry

save_addr :: Address -> IO ()
save_addr addr = putStrLn ("saving: " <> line) >> appendFile alias_file line
    where
        line = "alias " ++ from_address addr

showAlias :: Alias -> String
showAlias entry = unwords (ali:name entry:addr:["\n"])
    where
        ali :: String
        ali = case alias entry of
            Nothing -> (filter ((/=) ' ')) (name entry) & map toLower
            Just str -> str
        addr = entry & address & from_address & enclose
            where
                enclose = ('<' :) <&> (<> ">")


read_alias :: ByteString -> Maybe Sender
read_alias = ByteString.Char8.unpack <&> readMaybe


run_server :: Database -> IO Void
run_server database = do
    addr <- resolve
    void $ Exception.bracket (open addr) close (loop database)
    pure $ error "server is terminating."
    where
        resolve :: IO AddrInfo
        resolve = do
            getAddrInfo (Just hints) Nothing (Just port_number) <&> head
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


loop :: Database -> Socket -> IO Database
loop database sock = do
    (conn, _peer) <- accept sock
    new_db <- process_msg database conn
    loop new_db sock


process_msg :: Database -> Socket -> IO Database
process_msg database sock = do
    msg <- recv sock max_bytes
    ByteString.Char8.putStrLn $ msg
    case read_alias msg of
        Nothing -> do
            ByteString.Char8.putStrLn $ "invalid: " <> msg
            pure database
        Just sender -> case sender of
            AddressOnlySender addr -> if is_new_and_valid addr database
                then save_addr addr *> pure (insert database addr)
                else pure database
            NamedSender ali -> if is_new_and_valid (address ali) database
                then save ali *> pure (insert database (address ali))
                else pure database





-- when to normalize / extract?
-- an email address with capitals
-- (Dan.So@example.com)
-- must be saved that way to the database
-- so it must be sent that way from the client
-- but when compared for existing aliases,
-- then it needs to be case-insensitive?
normalize :: Address -> Address
normalize = extract <&> from_address <&> map toLower <&> Address
