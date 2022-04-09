module Settings (
    port_number,
    ignored,
) where

import Data.ByteString (ByteString)
import Network.Socket (ServiceName)

port_number :: ServiceName
port_number = "10345"

ignored :: [ByteString]
ignored = [
    "noreply",
    "no-reply",
    "donot-reply",
    "do-not-reply",
    "nepasrepondre"
    ]

