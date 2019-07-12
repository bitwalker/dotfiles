# Disable fish greeting
set fish_greeting

# Store history in a shared session by default, unless otherwise set
if not set -q fish_history
    set -gx fish_history "default"
else if test "$fish_history" = "fish"
    set fish_history "default"
end

# My terminal editor is always Vim by default
set -x EDITOR "vim"

# Initialize XDG_* environment variables/paths
require xdg
# Initialize PATH/MANPATH
require paths
# Initialize PATH/MANPATH extensions for GNU coreutils
require gnu

# Initialize third-party plugins (via fundle)
# These come before my custom plugins in case I want to make
# use of external helpers in my own plugins (i.e. bass)
fundle plugin 'edc/bass'
fundle plugin 'oh-my-fish/plugin-errno'
fundle init

# These are all my own custom plugins
# Most of these just do simple environment initialization
require tmuxp
require homebrew
require nix
require asdf
require rust
require erlang
require elixir
require ocaml
require java
require golang
require direnv 
require ccache
require llvmenv
require launchctl
