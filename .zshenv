typeset -gx EDITOR=vim
typeset -gx VISUAL=vim

typeset -gx XDG_DATA_DIRS=/usr/share:/usr/local/share
typeset -gx XDG_CONFIG_DIRS=/etc/xdg
typeset -gx XDG_CONFIG_HOME=$HOME/.config
typeset -gx XDG_CACHE_HOME=$HOME/.cache
typeset -gx XDG_DATA_HOME=$HOME/.local/share

typeset -gx ZDOTDIR=$XDG_CONFIG_HOME/zsh

typeset -gx FZF_DEFAULT_COMMAND="fd --follow"
typeset -gx FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
typeset -gx FZF_DEFAULT_OPTS='--height 40%'

path=('/usr/local/bin' '/usr/local/sbin' $path)
