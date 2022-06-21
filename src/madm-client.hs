import Data.ByteString qualified as BS
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
import System.Exit (exitFailure)

import Settings qualified
import MADM.Client.Message (names)
import MADM.Client.Network (send_msg)

main :: IO ()
main = do
    input <- BS.getContents
    BS.putStr input
    case names input of
        "" -> exitFailure
        text -> send_msg "127.0.0.1" Settings.port_number text
