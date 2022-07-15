module MADM.Client.Date (
    localized,
) where

import Data.Bool (not)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Eq
import Data.Function
import Data.Functor ((<&>))
import Data.List qualified as List

import Data.Time
import Control.Monad

parse_format :: String
parse_format = "Date: %a, %e %b %Y %T %z"

out_format :: String
out_format = "%a, %d %b 1%Y %T (dan time)"

localized :: ByteString -> IO ByteString
localized input = do
    tz <- getCurrentTimeZone
    pure $ case split input of
        Nothing -> input
        Just (start, date, rest) ->
            BS.Char8.unlines (start <> [converted date tz] <> rest)


split :: ByteString -> Maybe ([ByteString], ByteString, [ByteString])
split str = case str & BS.Char8.lines & List.break (BS.isPrefixOf "Date: ") of
    (_, []) -> Nothing
    (some, date:more) -> Just (some, date, more)


converted :: ByteString -> TimeZone -> ByteString
converted str tz = case parsed str of
    Nothing -> str
    Just time -> time
               & utcToLocalTime tz
               & formatted
               & BS.Char8.pack
               & ("Date: " <>)

parsed :: ByteString -> Maybe UTCTime
parsed = BS.Char8.unpack <&> parseTimeM True defaultTimeLocale parse_format

formatted :: LocalTime -> String
formatted t = formatTime defaultTimeLocale out_format t
