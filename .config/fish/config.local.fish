# Local variables
set BINPATH /usr/local/bin /usr/local/games/bin /usr/local/sbin /usr/local/Cellar /usr/bin /usr/sbin /bin
set RVM ~/.rvm/bin
set GNUUTILS /usr/local/opt/coreutils/libexec/gnubin
set GNUMAN /usr/local/opt/coreutils/libexec/gnuman
set HEROKU /usr/local/heroku/bin
set EXENV ~/.exenv/bin ~/.exenv/shims

# Exports
set -gx NVM_DIR ~/.nvm
set -x PATH $GNUUTILS $BINPATH $HEROKU $RVM $EXENV $NVM_DIR $PATH
set -x MANPATH $GNUMAN /usr/share/man
set -x EDITOR "vim"

# Init RVM
rvm >/dev/null

# Init exenv
exenv rehash >/dev/null
