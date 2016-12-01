# Local variables
set NPM_BIN node_modules/.bin
set N_PREFIX $HOME/n

# Exports
#bass source ~/.nix-profile/etc/profile.d/nix.sh

set -x GOPATH ~
set -x PATH $N_PREFIX/bin $NPM_BIN $GOPATH/bin /usr/local/bin /usr/local/sbin $PATH
set -x MANPATH /usr/local/share/man /usr/share/man
set -x EDITOR "vim"

# Init erlang
#source ~/erlang/19.1/activate.fish

# OPAM configuration
. /Users/paulschoenfelder/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
