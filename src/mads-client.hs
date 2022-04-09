{- todo:
    switch from String to Text or ByteString?
    bug: sometimes the From: line is super long and takes
         more than one line.
-}


-- import Data.Text
import System.Exit (exitFailure)
import Data.Char (toLower, isAlphaNum)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, find)

import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString (ByteString)
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
    Socket,
    socket,
    SocketType(Stream),
    withSocketsDo,
    )
import Network.Socket.ByteString (sendAll)

import Prelude hiding (log)

import Common

main :: IO ()
main = do
    input <- getContents
    case grepFrom input >>= fromLine of
        Nothing -> do
            putStr input
            exitFailure
        Just sender -> do
            send_msg (ByteString.Char8.pack (show sender))
            putStr input

generate_alias :: String -> String
generate_alias str = str <&> replace <&> toLower & hyphen_words & hyphen_unwords
    where
        replace :: Char -> Char
        replace c = if not (permitted c) then '-' else c

        permitted :: Char -> Bool
        permitted c = isAlphaNum c || is_hyphen c

        is_hyphen :: Char -> Bool
        is_hyphen = (== '-')

        -- words, but using hyphens instead of spaces.
        -- splitting the string into words, then re-assembling it
        -- is an easy way to ensure
        -- that there is only one hyphen between each word.
        hyphen_words :: String -> [String]
        hyphen_words s = case dropWhile is_hyphen s of
            "" -> []
            some -> w : hyphen_words rest
                where
                    (w, rest) = break is_hyphen some

        hyphen_unwords :: [String] -> String
        hyphen_unwords = \case
            [] -> ""
            ws -> foldr1 (\w s -> w ++ '-':s) ws


grepFrom :: String -> Maybe String
grepFrom = lines <&> find (isPrefixOf "From") <&> fmap removeQuotes
    where
        removeQuotes :: String -> String
        removeQuotes = filter ((/=) '"')

fromLine :: String -> Maybe Sender
fromLine = \case
    'F':'r':'o':'m':':' : line -> Just $ if null nam
        then addr & AddressOnlySender
        else Alias ali nam addr & NamedSender
            where
                separated = words line
                ali = Just (generate_alias nam)
                nam = init separated & unwords
                addr = last separated & Address & extract
    _ -> Nothing

send_msg :: ByteString -> IO ()
send_msg msg = runTCPClient "127.0.0.1" "3000" $ \sock ->
    sendAll sock msg

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    Exception.bracket (open addr) close client
    where
        resolve = do
            let hints = defaultHints {
                addrSocketType = Stream
                }
            head <$> getAddrInfo (Just hints) (Just host) (Just port)
        open addr = do
            sock <- socket
                (addrFamily addr)
                (addrSocketType addr)
                (addrProtocol addr)
            connect sock $ addrAddress addr
            pure sock
