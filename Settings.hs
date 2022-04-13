module Settings (
    port_number,
    max_msg_length,
    ignored,
) where

import Data.ByteString (ByteString)
import Network.Socket (ServiceName)

port_number :: ServiceName
port_number = "10345"

-- the maximum length of the message, in bytes.
max_msg_length :: Int
max_msg_length = 8192

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
