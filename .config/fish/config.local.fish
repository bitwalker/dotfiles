# Local variables
set NPM_BIN node_modules/.bin
set EXENV ~/.exenv/bin ~/.exenv/shims
set RBENV ~/.rbenv/bin ~/.rbenv/shims
set N_PREFIX $HOME/n

# Exports
set -x GOPATH ~
set -x PATH $N_PREFIX/bin $EXENV $RBENV $NPM_BIN $GOPATH/bin /usr/local/bin $PATH
set -x MANPATH /usr/local/share/man /usr/share/man
set -x EDITOR "vim"

# Init exenv
exenv rehash >/dev/null


eval (direnv hook fish)
