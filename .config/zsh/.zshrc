# Use Emacs bindings for the shell
bindkey -e

typeset -gx HISTFILE=$HOME/.config/zsh/.zsh_history
typeset -gx HISTSIZE=10000
typeset -gx SAVEHIST=100000

# Share history across sessions
setopt append_history
unsetopt share_history
unsetopt inc_append_history

# Handle duplicates/superfluous commands in history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_find_no_dups
setopt hist_save_no_dups
setopt hist_no_store
setopt hist_ignore_space

# Enable comments in the prompt
setopt interactive_comments

# Disable '<Ctrl>D' to logout
setopt ignore_eof

# Disable sound
unsetopt beep

typeset -gx TERM=xterm-24bit

[[ -d $HOME/bin ]] || mkdir -p $HOME/bin

path=('/usr/local/bin' $path)
typeset -TUxg MANPATH manpath=('/usr/local/share/man' '/usr/share/man')
typeset -TUxg INFOPATH infopath

# Add GNU coreutils to PATH if present
if [[ -d /usr/local/opt/coreutils/libexec/gnubin ]]; then
    path=('/usr/local/opt/coreutils/libexec/gnubin' $path)
fi
if [[ -d /usr/local/opt/coreutils/libexec/gnuman ]]; then
    manpath=('/usr/local/opt/coreutils/libexec/gnuman' $manpath)
    manpath=('/usr/local/opt/coreutils/share/man' $manpath)
fi

# Put personal bin directory before all others
path=("$HOME/bin" $path)

# Use directory-sensitive history
source $XDG_CONFIG_HOME/zsh/plugins/directory-history.zsh

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Initialize completions
autoload -U compinit
compinit

# Load theme
source $XDG_DATA_HOME/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Homebrew
typeset -gx HOMEBREW_NO_ANALYTICS=1
if [[ -f "$XDG_CONFIG_HOME/brew/API_TOKEN" ]]; then
    typeset -gx HOMEBREW_GITHUB_API_TOKEN="$(cat "$XDG_CONFIG_HOME/brew/API_TOKEN")"
fi

# Initialize ASDF
if [[ -f ~/.asdf/asdf.sh ]]; then
    source ~/.asdf/asdf.sh
fi

# When available, make use of ccache
if type -p ccache >/dev/null; then
    typeset -gx USE_CCACHE=1
fi

# Set common flags for Elixir/Erlang
typeset -gx ELIXIR_EDITOR="emacsclient -a 'vim' +__LINE__ __FILE__"
typeset -gx ERL_AFLAGS="-kernel shell_history enabled"

# Set GOPATH
typeset -gx GOPATH=$HOME

# Java
_java=`asdf current java 2>&1 >/dev/null`
if [[ $? -eq 0 ]]; then
    typeset -gx JAVA_HOME=$(asdf where java)
fi
if [[ -z "$JAVA_HOME" ]] && [[ -f /usr/libexec/java_home ]]; then
    typeset -gx JAVA_HOME="$(/usr/libexec/java_home)"
fi

# Rust
if [[ -d "$HOME/.cargo/bin" ]]; then
    path=($HOME/.cargo/bin $path)
fi

# direnv
if ! type -p _direnv_hook >/dev/null; then
    eval "$(direnv hook zsh)";
fi

# Ensure PATH is exposed to GUI apps
if type -p launchctl >/dev/null; then
    launchctl setenv PATH "$(echo "$PATH" | sed -e 's| /|:/|g' -e 's| ./|:./|g')"
fi

zmodload zsh/datetime

# When changing to a directory with .node_modules/bin, add it to the PATH
_node_modules_hook() {
    if [[ -d .node_modules/bin ]]; then
        path=(.node_modules/bin $path)
    elif [[ -n "${path[(r).node_modules/bin]}" ]]; then
        path-=.node_modules/bin
    fi
}

# When changing to a directory with .tool-versions, update JAVA_HOME
_tool_versions_hook() {
    if [[ -f .tool-versions ]]; then
        if asdf current java 2>&1 > /dev/null; then
            typeset -gx JAVA_HOME=$(asdf where java)
        fi
    fi
}

typeset -ag chpwd_functions;
if [[ -z ${chpwd_functions[(r)_node_modules_hook]} ]]; then
    chpwd_functions+=_node_modules_hook;
fi
if [[ -z ${chpwd_functions[(r)_tool_versions_hook]} ]]; then
    chpwd_functions+=_tool_versions_hook;
fi

_kill_emacs() {
    ps -cx | grep '[Ee]macs' | sed -e 's/^[ ]*//' | head -n1 | cut -d' ' -f1 | xargs kill
}
alias killemacs='_kill_emacs'
alias vim='nvim'

# Allow alt+w to delete path segments on the command line
backward-kill-dir () {
    local WORDCHARS=${WORDCHARS/\/}
    zle backward-kill-word
}
zle -N backward-kill-dir
bindkey '^[w' backward-kill-dir

# Bind up/down arrow keys to navigate through history
bindkey '^[[B' directory-history-search-backward
bindkey '^[[A' directory-history-search-forward

# Bind CTRL+k and CTRL+j to substring search
bindkey '^j' history-substring-search-up
bindkey '^k' history-substring-search-down
