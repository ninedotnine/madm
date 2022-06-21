# mutt alias danabase manager

`madm` is a server for managing mutt aliases.

## background

[mutt](http://mutt.org) is my email client of choice.

Unfortunately, mutt has no built-in feature
for automatically adding new email addresses
to an address book. Many users choose to add their own,
[as described here](http://wcaleb.org/blog/mutt-tips).

A common trick is to use mutt's `display_filter` setting
to run an arbitrary program on a message.
This program could, for example,
scan it for things that appear to be email addresses
and add them to a database automatically.

One drawback to this hack is that the program must finish
before the message can be displayed to the reader.
If you want to check whether an email address
has been seen before, then your `display_filter`
will need to open and parse your entire aliases database
every time mutt displays a message.

This is pathologically inefficient.

The solution is to load the database in the background
in another process, and then use the `display_filter`
to send the address to that process.

This is what `madm` does.

## building

`make`.

If the build fails with an error such as "Could not find module ‘Prelude’. There are files missing in the 'base' package" then try:

`make HSFLAGS=-static`

## configuration

`madm` is configured by modifying `Settings.hs` and rebuilding.

## usage

`bin/madm-server &`

alternatively,

`systemctl --user start --now madm.service`

then add this line to your `.muttrc`:

`set display_filter=madm-client`
