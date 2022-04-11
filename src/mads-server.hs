import Control.Exception qualified as Exception
import Control.Monad (void, foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Word8 (Word8)
import Data.Word8 qualified as Word8
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (mapAccumL)
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

import Types
import Settings qualified as Settings
import Database (Database)
import Database qualified as Database
import Address

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
                      <&> BS.split Word8._comma
                      <&> squashed_commas
                      <&> map BS.Char8.strip
                      <&> map parsed
                      <&> partitionEithers
    mapM_ report invalids
    foldM (Database.update aliases_file) database valids


-- if one of the strings does not contain an email address
-- then it must have been split because there was a comma in the name.
-- this function sticks concats every string in the list
-- that does not appear to have an email address
-- (checked by whether it contains an '@')
-- and concats that with the next string in the list.
squashed_commas :: [ByteString] -> [ByteString]
squashed_commas = accum
              <&> snd
              <&> filter (/= BS.empty)
    where
        accum :: [ByteString] -> (ByteString, [ByteString])
        accum = mapAccumL (\s x ->
            if has_email_address x
                then ("", s <> x)
                else (s <> x <> ",", "")
            ) ""
        has_email_address name = Word8._at `BS.elem` name


parsed :: ByteString -> Either ByteString Contact
parsed line = case BS.Char8.words line of
    [] -> Left ""
    [email] -> email
             & Address
             & extracted
             & AddressOnly
             & Right
    separated -> Right $ if BS.null name
        then AddressOnly addr
        else Named nick name addr
            where
                nick = if BS.elem Word8._at name
                    then Nothing
                    else Just $ generate_nick name
                name = init separated & BS.Char8.unwords
                addr = last separated & Address & extracted


                generate_nick :: ByteString -> ByteString
                generate_nick = BS.map replace
                            <&> BS.map Word8.toLower
                            <&> hyphen_words
                            <&> hyphen_unwords
                    where
                        replace :: Word8 -> Word8
                        replace c = if not (permitted c)
                            then Word8._hyphen
                            else c

                        permitted :: Word8 -> Bool
                        permitted c = Word8.isAlphaNum c
                                   || is_hyphen c

                        is_hyphen :: Word8 -> Bool
                        is_hyphen = (== Word8._hyphen)

                        -- words, but using hyphens instead of spaces.
                        -- splitting the string into words, then
                        -- re-assembling it is an easy way to ensure
                        -- that there is only one hyphen between each word.
                        hyphen_words :: ByteString -> [ByteString]
                        hyphen_words s = case BS.dropWhile is_hyphen s of
                            "" -> []
                            some -> w : hyphen_words rest
                                where
                                    (w, rest) = BS.break is_hyphen some

                        hyphen_unwords :: [ByteString] -> ByteString
                        hyphen_unwords = \case
                            [] -> ""
                            ws -> foldr1 (\w s -> w <> "-" <> s) ws


report :: ByteString -> IO ()
report msg = BS.Char8.putStrLn $ "invalid: " <> msg


removeQuotes :: ByteString -> ByteString
removeQuotes = BS.filter (/= Word8._quotedbl)

