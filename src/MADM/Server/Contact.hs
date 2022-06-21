module MADM.Server.Contact (
    Contact(..),
    Address(..),
    address,
    extracted,
) where

import Data.ByteString (ByteString)

import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Word8 qualified as Word8

data Contact = Named (Maybe ByteString) ByteString Address
            | AddressOnly Address
    deriving (Eq, Ord)


newtype Address = Address {
    from_address :: ByteString
    } deriving (Eq, Ord)


address :: Contact -> Address
address = \case
    Named _ _ addr -> addr
    AddressOnly addr -> addr


extracted :: Address -> Address
extracted = from_address
        <&> BS.filter (`notElem` angle_brackets)
        <&> Address
    where
        angle_brackets = [Word8._less, Word8._greater]
