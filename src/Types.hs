module Types where

import Data.ByteString (ByteString)


data Sender = Named (Maybe ByteString) ByteString Address
            | AddressOnly Address
    deriving (Eq, Ord, Read, Show)

newtype Address = Address {
    from_address :: ByteString
    } deriving (Eq, Ord, Read, Show)

address :: Sender -> Address
address = \case
    Named _ _ addr -> addr
    AddressOnly addr -> addr

