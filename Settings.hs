module Settings (
    port_number,
    max_bytes,
    ignored,
) where

import Data.ByteString (ByteString)
import Network.Socket (ServiceName)

port_number :: ServiceName
port_number = "10345"

-- the maximum length of the message, in bytes.
max_bytes :: Int
max_bytes = 8192

-- do not add aliases whose names
-- contain any of these substrings.
ignored :: [ByteString]
ignored = [ "noreply"
          , "no-reply"
          , "donot-reply"
          , "do-not-reply"
          , "nepasrepondre"
          , "ne-pas-repondre"
          ]
