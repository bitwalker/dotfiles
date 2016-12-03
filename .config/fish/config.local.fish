# Local variables
set NPM_BIN node_modules/.bin
set N_PREFIX $HOME/n
set CARGO_BIN $HOME/.cargo/bin

# Exports
#bass source ~/.nix-profile/etc/profile.d/nix.sh

set -x GOPATH ~
set -x PATH $N_PREFIX/bin $NPM_BIN $GOPATH/bin $CARGO_BIN /usr/local/bin /usr/local/sbin $PATH
set -x MANPATH /usr/local/share/man /usr/share/man
set -x EDITOR "vim"

# Init package managers
source ~/.asdf/asdf.fish

# OPAM configuration
. /Users/paulschoenfelder/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
