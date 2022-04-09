module Settings (
    alias_file,
    port_number,
    log_file,
) where

import Network.Socket ( ServiceName )

alias_file :: FilePath
alias_file = "/home/user/.mutt/aliases"

port_number :: ServiceName
port_number = "3000"

log_file :: FilePath
log_file = "/tmp/mutt-database-server.log"
