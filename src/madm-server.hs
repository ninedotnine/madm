import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import MADM.Server.Database qualified as Database
import MADM.Server.Server qualified as Server

main :: IO Void
main = do
    aliases_file <- getArgs
                >>= \case
                    [arg] -> pure arg
                    _ -> BS.Char8.putStrLn usage >> exitFailure
                        where
                            usage = "usage: mads-server filename"
    putStrLn $ ["loading ", aliases_file, "..."] & concat
    database <- BS.readFile aliases_file
            <&> Database.parsed
    Server.run aliases_file database
