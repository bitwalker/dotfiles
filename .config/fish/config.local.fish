# Disable fish greeting
set fish_greeting

# Navigation
function ll ; tree --dirsfirst -ChFupDaLg 1 $argv ; end

# XDG System
set -x XDG_DATA_DIRS /usr/share /usr/local/share
set -x XDG_CONFIG_DIRS /etc/xdg
# XDG User
set -x XDG_CACHE_HOME $HOME/.cache
set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_DATA_HOME $HOME/.local/share
set -x XDG_DESKTOP_DIR $HOME/Desktop
set -x XDG_DOWNLOAD_DIR $HOME/Downloads
set -x XDG_DOCUMENTS_DIR $HOME/Documents
set -x XDG_MUSIC_DIR $HOME/Music
set -x XDG_PICTURES_DIR $HOME/Pictures
set -x XDG_VIDEOS_DIR $HOME/Videos

set -x GNU_PATH /usr/local/opt/coreutils/libexec/gnubin
set -x GNU_MANPATH /usr/local/opt/coreutils/libexec/gnuman
set -x PATH $GNU_PATH /usr/local/bin /usr/local/sbin $PATH
set -x MANPATH $GNU_MANPATH /usr/local/share/man /usr/share/man $MANPATH
set -x EDITOR "vim"

# Tmux
set -x TMUXP_CONFIGDIR $XDG_CONFIG_HOME/tmuxp

# Homebrew
set -x HOMEBREW_NO_ANALYTICS 1
if test -f $XDG_CONFIG_HOME/brew/API_TOKEN
  set -l token (cat $XDG_CONFIG_HOME/brew/API_TOKEN)
  set -x HOMEBREW_GITHUB_API_TOKEN "$token"
end

# Init package managers
# nix
#bass source ~/.nix-profile/etc/profile.d/nix.sh
source ~/.asdf/asdf.fish

# erlang/elixir
set -x ERL_AFLAGS "-kernel shell_history enabled"
set -x ELIXIR_EDITOR "~/bin/emacs +__LINE__ __FILE__"

# rust
if test -d ~/.cargo/bin
  set -x PATH ~/.cargo/bin $PATH
end

# golang
set -x GOPATH ~
mkdir -p $GOPATH/bin
set -x PATH $GOPATH/bin $PATH

#java
if test -f /usr/libexec/java_home
  set -x JAVA_HOME (/usr/libexec/java_home)
end

# opam
if hash opam 2>/dev/null
  eval (opam config env)
end

# expose PATH to graphical apps
if which launchctl >/dev/null
  launchctl setenv PATH (echo $PATH | sed -e 's| /|:/|g' -e 's| ./|:./|g')
end

# Fish plugins
fundle plugin 'bitwalker/pure'
fundle plugin 'edc/bass'
fundle plugin 'tuvistavie/oh-my-fish-core'
fundle plugin 'oh-my-fish/plugin-errno'

fundle init
