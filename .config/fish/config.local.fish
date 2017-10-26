# Local variables
set NPM_BIN node_modules/.bin
set CARGO_BIN $HOME/.cargo/bin

# Exports
#bass source ~/.nix-profile/etc/profile.d/nix.sh

set -x GOPATH ~
set -x GNUPATH /usr/local/opt/make/libexec/gnubin
set -x PATH $NPM_BIN $GOPATH/bin $CARGO_BIN $GNUPATH /usr/local/bin /usr/local/sbin $PATH
set -x MANPATH /usr/local/share/man /usr/share/man
set -x EDITOR "vim"

set -x ERL_AFLAGS "-kernel shell_history enabled"
set -x ELIXIR_EDITOR "~/bin/emacs +__LINE__ __FILE__"

# Init package managers
source ~/.asdf/asdf.fish

# OPAM configuration
. /Users/paulschoenfelder/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
