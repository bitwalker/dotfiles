# Disable fish greeting
set fish_greeting

# Navigation
function ll ; tree --dirsfirst -ChFupDaLg 1 $argv ; end

set -x XDG_CONFIG_HOME ~/.config
set -x GNU_PATH /usr/local/opt/coreutils/libexec/gnubin
set -x GNU_MANPATH /usr/local/opt/coreutils/libexec/gnuman
set -x PATH $GNU_PATH /usr/local/bin /usr/local/sbin $PATH
set -x MANPATH $GNU_MANPATH /usr/local/share/man /usr/share/man $MANPATH
set -x EDITOR "vim"

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
