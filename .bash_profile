#!/usr/bin/env bash

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

# Load bin paths
export BINPATH=/bin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/local/Cellar
export RVM=~/.rvm/bin
export GNUUTILS="/usr/local/opt/coreutils/libexec/gnubin"
export GNUMAN="/usr/local/opt/coreutils/libexec/gnuman"
export PATH=$PATH:$BINPATH:$RVM:$GNUUTILS
export MANPATH=$GNUMAN:$MANPATH
export GOPATH=~/Repositories/Go

# Path to the bash it configuration
export BASH_IT=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
#export BASH_IT_THEME='minimal'
export BASH_IT_THEME='modern'

# Set my editor and git editor
export EDITOR="/bin/subl -w"
export GIT_EDITOR='/usr/bin/vim -w'

# Don't check mail when opening terminal.
unset MAILCHECK

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# Load Bash It
source $BASH_IT/bash_it.sh

# Smarter Autocompletion
bind 'set completion-ignore-case on'
bind 'set show-all-if-ambigous on'
bind 'TAB:menu-complete'
bind 'set visible-stats on'
shopt -s cdspell

# Load bash-completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# NVM - Node.js Verson Manager
. ~/.nvm/nvm.sh

# Directory Bookmarks
export CDPATH=~/Repositories:~/Repsitories/Shared

# Aliases

# ls is always in long form, follows symbolic links, filters out . and .., displays
# files starting with '.', shows block usage in kb, and prints '/' after directories,
# and output is colorized.
alias ls="ls -lAGHksp"

# For easy .jar execution
alias runjar="java -jar "
