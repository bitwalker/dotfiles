# Local variables
set NPM_BIN node_modules/.bin
set EXENV ~/.exenv/bin ~/.exenv/shims
set N_PREFIX $HOME/n

# Exports
#bass source ~/.nix-profile/etc/profile.d/nix.sh

set -x GOPATH ~
set -x PATH $N_PREFIX/bin $EXENV $NPM_BIN $GOPATH/bin /usr/local/bin /usr/local/sbin $PATH
set -x MANPATH /usr/local/share/man /usr/share/man
set -x EDITOR "vim"

# Init exenv
exenv rehash >/dev/null

eval (direnv hook fish)

# Init erlang
source ~/erlang/19.0/activate.fish
