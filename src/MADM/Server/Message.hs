module MADM.Server.Message (
    parsed,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (mapAccumL)
import Data.Word8 (Word8)
import Data.Word8 qualified as Word8

import MADM.Server.Contact

parsed :: ByteString -> ([ByteString], [Contact])
parsed = split
     <&> map BS.Char8.strip
     <&> map line_parsed
     <&> partitionEithers


split :: ByteString -> [ByteString]
split = BS.split Word8._comma
    <&> commas_squashed


-- if one of the strings does not contain an email address
-- then it must have been split because there was a comma in the name.
-- this function sticks concats every string in the list
-- that does not appear to have an email address
-- (checked by whether it contains an '@')
-- and concats that with the next string in the list.
commas_squashed :: [ByteString] -> [ByteString]
commas_squashed = accum
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


line_parsed :: ByteString -> Either ByteString Contact
line_parsed line = case BS.Char8.words line of
    [] -> Left ""
    [email] -> email
             & Address
             & extracted
             & AddressOnly
             & Right
    separated -> let
        -- these are safe: `separated` must have at least 2 items
        name = init separated & BS.Char8.unwords
        addr = last separated & Address & extracted
        in Right $ if BS.null name || is_address (head separated)
            then AddressOnly addr
            else let
                nick = if BS.elem Word8._at name
                    then Nothing
                    else Just $ generated_nick name
                in Named nick name addr


is_address :: ByteString -> Bool
is_address name = "\"" `BS.isPrefixOf` name
               && Word8._at `BS.elem` name
               && Word8._period `BS.elem` name


generated_nick :: ByteString -> ByteString
generated_nick = BS.map replace
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
