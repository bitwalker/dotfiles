# Local variables
set BINPATH /usr/local/bin /usr/local/sbin /usr/local/Cellar /usr/bin /usr/sbin /bin
set RVM ~/.rvm/bin
set EXENV ~/.exenv/bin ~/.exenv/shims

# Exports
set -gx NVM_DIR ~/.nvm
set -x GOPATH ~/
set -x PATH $BINPATH $RVM $EXENV $NVM_DIR $GOPATH/bin $PATH
set -x MANPATH /usr/local/share/man /usr/share/man
set -x EDITOR "vim"

# Init RVM
rvm >/dev/null

# Init exenv
exenv rehash >/dev/null

eval (direnv hook fish)
