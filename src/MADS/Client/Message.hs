module MADS.Client.Message (
    names,
) where

import Prelude ()
import Data.Bool (not)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Eq
import Data.Function
import Data.Functor ((<&>))
import Data.List qualified as List

data Header = From | To | CC | ReplyTo

header_string :: Header -> ByteString
header_string = \case
    From -> "From: "
    To -> "To: "
    CC -> "CC: "
    ReplyTo -> "Reply-To: "

names :: ByteString -> ByteString
names txt = [
    grep From,
    grep To,
    grep CC,
    grep ReplyTo
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
                    & (BS.drop len first :)
                    & BS.Char8.unwords
    where
        len = BS.length header
        header = header_string hdr
