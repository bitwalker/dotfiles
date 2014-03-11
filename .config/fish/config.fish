# Load paths
set BINPATH /usr/local/bin /usr/local/sbin /usr/local/Cellar /usr/bin /usr/sbin /bin
set RVM ~/.rvm/bin
set GNUUTILS /usr/local/opt/coreutils/libexec/gnubin
set GNUMAN /usr/local/opt/coreutils/libexec/gnuman
set HEROKU /usr/local/heroku/bin
set GOPATH ~/Repositories/Go

set -x PATH $GNUUTILS $BINPATH $HEROKU $RVM $PATH
set -x MANPATH $GNUMAN $MANPATH

set -x EDITOR "vim"
set -x JAVA_HOME "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home"

# Remove fish greeting
set fish_greeting ""

# Pull in machine-local, git-ignored config file
if test -f ~/.config/fish/config.local.fish
    . ~/.config/fish/config.local.fish
end

alias ls "ls -lAGHksp"
alias todo "todo.sh"

rvm >/dev/null
