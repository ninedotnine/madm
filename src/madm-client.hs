import Data.ByteString qualified as BS
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
import System.Exit (exitFailure)

import Settings qualified
import MADM.Client.Message (names)
import MADM.Client.Network (send_msg)
import MADM.Client.Date (localized)

main :: IO ()
main = do
    input <- BS.getContents
    localized input >>= BS.putStr
    case names input of
        "" -> exitFailure
        text -> send_msg "127.0.0.1" Settings.port_number text
