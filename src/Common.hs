-- module Common (
--     Sender(..),
--     Alias(..),
--     Address(..),
-- ) where

module Common where

import Data.Functor ((<&>))
import Data.Char (toLower, isSpace, isAlphaNum)

data Sender = NamedSender Alias
            | AddressOnlySender Address
    deriving (Eq, Read, Show)

data Alias = Alias {
    alias :: Maybe String,
    name :: String,
    address :: Address
    } deriving (Eq, Read, Show)

newtype Address = Address {
    from_address :: String
    } deriving (Eq, Read, Show)

extract :: Address -> Address
extract = from_address <&> filter (\c -> notElem c ['<','>']) <&> Address
