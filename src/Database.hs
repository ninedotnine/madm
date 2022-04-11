module Database (
    Database,
    parsed,
    update,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Word8 qualified as Word8

import Data.Char (isSpace)

import Contact
import Settings
import Contact

type Database = Set Address

parsed :: ByteString -> Database
parsed = BS.Char8.lines
     <&> filter (not . BS.Char8.all isSpace)
     <&> map addr
     <&> Set.fromList
    where
        addr = BS.Char8.words <&> last <&> Address <&> normalized


update :: FilePath -> Database -> Contact -> IO Database
update aliases_file database sender = do
    if is_new_and_not_ignored (address sender) database
        then save aliases_file database sender
        else pure database


is_new_and_not_ignored :: Address -> Database -> Bool
is_new_and_not_ignored addr db = is_new
                              && is_not_ignored
    where
        is_new = not $ db `contains` addr
        is_not_ignored = not $ or $
            (`BS.isInfixOf` from_address addr) <$> Settings.ignored


contains :: Database -> Address -> Bool
contains database addr = normalized addr `Set.member` database


save :: FilePath -> Database -> Contact -> IO Database
save aliases_file database sender = do
    BS.Char8.putStrLn ("saving: " <> line)
    BS.appendFile aliases_file line
    pure (Set.insert (address sender) database)
        where
            line = rendered sender

-- an email address with capitals
-- must be saved that way to the database.
-- but when checking for existing aliases,
-- the comparison should be case-insensitive
normalized :: Address -> Address
normalized = extracted
         <&> from_address
         <&> BS.map Word8.toLower
         <&> Address


rendered :: Contact -> ByteString
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

