module Settings (
    port_number,
    max_bytes,
    ignored,
) where

import Data.ByteString (ByteString)
import Network.Socket (ServiceName)

port_number :: ServiceName
port_number = "10345"

max_bytes :: Int
max_bytes = 8192

ignored :: [ByteString]
ignored = [
    "noreply",
    "no-reply",
    "donot-reply",
    "do-not-reply",
    "nepasrepondre"
    ]

