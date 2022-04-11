module MADS.Server.Database (
    Database,
    parsed,
    update,
) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Word8 qualified as Word8

import MADS.Server.Contact
import Settings

type Database = Set Address

parsed :: ByteString -> Database
parsed = BS.Char8.lines
     <&> filter (not . BS.Char8.all isSpace)
     <&> map addr
     <&> Set.fromList
    where
        addr = BS.Char8.words <&> last <&> Address <&> normalized


update :: Foldable t => FilePath -> Database -> t Contact -> IO Database
update = foldM <$> check_and_update


check_and_update :: FilePath -> Database -> Contact -> IO Database
check_and_update aliases_file database sender = do
    if is_new_and_not_ignored (address sender) database
        then save aliases_file database sender
        else pure database


is_new_and_not_ignored :: Address -> Database -> Bool
is_new_and_not_ignored addr db = is_new
                              && is_not_ignored
    where
        is_new = normalized addr `Set.notMember` db
        is_not_ignored = not $ or $
            (`BS.isInfixOf` from_address addr) <$> Settings.ignored


save :: FilePath -> Database -> Contact -> IO Database
save aliases_file database sender = do
    BS.appendFile aliases_file (rendered sender)
    pure (Set.insert (address sender) database)


rendered :: Contact -> ByteString
rendered = \case
    Named (Just nick) name addr ->
        "alias " <> BS.Char8.unwords [nick, name, enclosed addr] <> "\n"
    Named Nothing name addr ->
        "alias " <> BS.Char8.unwords [name, enclosed addr] <> "\n"
    AddressOnly addr ->
        "alias " <> from_address addr <> "\n"
  where
    enclosed :: Address -> ByteString
    enclosed = from_address
           <&> \addr -> BS.concat ["<" , addr , ">"]


-- an email address with capitals
-- must be saved that way to the database.
-- but when checking for existing aliases,
-- the comparison should be case-insensitive
normalized :: Address -> Address
normalized = extracted
         <&> from_address
         <&> BS.map Word8.toLower
         <&> Address
