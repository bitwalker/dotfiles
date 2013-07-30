#!/usr/bin/env bash

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

# Load bin paths
export BINPATH=/usr/local/bin:/usr/local/sbin:/usr/local/Cellar:/usr/bin:/usr/sbin:/bin
export RVM=~/.rvm/bin
export GNUUTILS="/usr/local/opt/coreutils/libexec/gnubin"
export GNUMAN="/usr/local/opt/coreutils/libexec/gnuman"
export PLAY="/usr/local/play-2.1.1"
export HEROKU="/usr/local/heroku/bin"
export PATH=$GNUUTILS:$BINPATH:$HEROKU:$RVM:$PLAY:$PATH
export MANPATH=$GNUMAN:$MANPATH
export GOPATH=~/Repositories/Go

# Path to the bash it configuration
export BASH_IT=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME='modern'

# Set my editor and git editor
#export EDITOR='subl -w'
export EDITOR='vim'
export GIT_EDITOR='vim -w'

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

# NVM - Node.js Verson Manager
. ~/.nvm/nvm.sh

# Aliases

# ls is always in long form, follows symbolic links, filters out . and .., displays
# files starting with '.', shows block usage in kb, and prints '/' after directories,
# and output is colorized.
alias ls="ls -lAGHksp"

# For easy .jar execution
alias runjar="java -jar "

# Easily run synchosts.py
alias syncvm="~/synchosts.py -n 'Windows 7' -u 'pschoenf' -p 'TH9ima4v'"

# Generate a gif from a series of .png images
function creategif {
  for f in *.png; do 
    convert "$f" -flatten -pointsize 20 -gravity south -annotate +0+25 "${f%.png}" "x$f"; 
  done
  local delayBy=200
  local filename="output.gif"
  convert -delay ${$1:-delayBy} x*.png ${$2:-filename}.gif;
}
