# mutt alias danabase server

`mads` is a server for managing mutt aliases.

## background

mutt unfortunately has no built-in feature
for automatically adding new email addresses
to an address book.

one common trick is to use mutt's `display_filter` setting
to run an arbitrary program on a message.
this program could, for example,
scan it for things that appear to be email addresses
and add them to a database automatically.

one drawback to this hack is that the program must finish
before the message can be displayed to the reader.
if you want to check whether an email address
has been seen before,
your `display_filter` will need to parse
the entire aliases file
every time mutt displays a message.

this is pathologically inefficient.

one solution is to load the database in the background
in another process,
and then send the address to that process.

this is what `mads` does.


## installation

try `make HSFLAGS=-static`

## configuration

`mads` is configured by modifying `src/Settings.hs` and rebuilding.

## usage

`bin/mads-server &`

alternatively,

`systemctl --user start --now mads.service`

then add this line to your `.muttrc`:

`set display_filter=mads-client`
