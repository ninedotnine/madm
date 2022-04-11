import Control.Exception qualified as Exception
import Control.Monad (void, foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Word8 (Word8)
import Data.Word8 qualified as Word8
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (mapAccumL)
import Data.Set qualified as Set
import Data.Set (Set)
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
import Settings qualified

max_bytes :: Int
max_bytes = 8192

type Database = Set Address

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
            <&> parsed_database
    run_server aliases_file database


run_server :: FilePath -> Database -> IO Void
run_server aliases_file database = do
    addr <- resolve
    void $ Exception.bracket (open addr) close (loop aliases_file database)
    pure $ error "server is terminating."
    where
        resolve :: IO AddrInfo
        resolve = do
            getAddrInfo (Just hints) Nothing (Just Settings.port_number) <&> head
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
    msg <- recv sock max_bytes
    BS.Char8.putStrLn msg

-- --     (invalids, valids) <- recv sock max_bytes
-- --                       <&> BS.split Word8._comma
--                          <&> squashed_commas
--                          <&> map BS.Char8.strip
-- --                       <&> map parsed
-- --                       <&> partitionEithers
    let (invalids, valids) = msg
                      & BS.split Word8._comma
                      & squashed_commas
                      & map BS.Char8.strip
                      & map parsed
                      & partitionEithers
    mapM_ report invalids
    foldM (update_database aliases_file) database valids


parsed_database :: ByteString -> Database
parsed_database = BS.Char8.lines
              <&> filter (not . BS.Char8.all isSpace)
              <&> map addr
              <&> Set.fromList
    where
        addr = BS.Char8.words <&> last <&> Address <&> normalized


is_new_and_valid :: Address -> Database -> Bool
is_new_and_valid addr db = is_valid && is_new
    where
        is_new = not $ db `contains` addr
        is_valid = not $ or $
            (`BS.isInfixOf` from_address addr) <$> Settings.ignored


contains :: Database -> Address -> Bool
contains database addr = normalized addr `Set.member` database


save :: FilePath -> Database -> Sender -> IO Database
save aliases_file database sender = do
    BS.Char8.putStrLn ("saving: " <> line)
    BS.appendFile aliases_file line
    pure (Set.insert (address sender) database)
        where
            line = rendered sender


rendered :: Sender -> ByteString
rendered = \case
    Named (Just nick) name addr ->
        "alias " <> BS.Char8.unwords [nick, name, enclosed addr] <> "\n"
    Named Nothing name addr ->
        "alias " <> BS.Char8.unwords [name, enclosed addr] <> "\n"
    AddressOnly addr ->
        "alias " <> enclosed addr
  where
    enclosed :: Address -> ByteString
    enclosed = from_address
           <&> \addr -> BS.concat ["<" , addr , ">"]


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


parsed :: ByteString -> Either ByteString Sender
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

update_database :: FilePath -> Database -> Sender -> IO Database
update_database aliases_file database sender = do
    if is_new_and_valid (address sender) database
        then save aliases_file database sender
        else pure database


removeQuotes :: ByteString -> ByteString
removeQuotes = BS.filter (/= Word8._quotedbl)

-- an email address with capitals
-- must be saved that way to the database.
-- but when checking for existing aliases,
-- the comparison should be case-insensitive
normalized :: Address -> Address
normalized = extracted
         <&> from_address
         <&> BS.map Word8.toLower
         <&> Address

extracted :: Address -> Address
extracted = from_address
        <&> BS.filter (`notElem` angle_brackets)
        <&> Address
    where
        angle_brackets = [Word8._less, Word8._greater]
