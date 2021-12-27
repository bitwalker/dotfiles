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
setopt hist_fcntl_lock
setopt hist_no_functions

# Enable comments in the prompt
setopt interactive_comments

# Disable '<Ctrl>D' to logout
setopt ignore_eof

# Disable sound
unsetopt beep

typeset -gx TERM=xterm-kitty
typeset -gx EMACSDIR="$XDG_CONFIG_HOME/emacs"
typeset -gx DOOMDIR="$XDG_CONFIG_HOME/doom"
typeset -gx DOOMLOCALDIR="$XDG_DATA_HOME/doom"

[[ -d $HOME/bin ]] || mkdir -p $HOME/bin

typeset -TUxg MANPATH manpath=('/usr/share/man' $manpath)
typeset -TUxg INFOPATH infopath=($infopath)

# Ensure Homebrew is initialized when present, unless already initialized
if [ -z "$HOMEBREW_PREFIX" ]; then
    if [[ -f /opt/homebrew/bin/brew ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
fi

# Prefer GNU coreutils when available
if [[ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin" ]]; then
    path=("$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin" $path)
fi
if [[ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman" ]]; then
    manpath=("$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman" $manpath)
    manpath=("$HOMEBREW_PREFIX/opt/coreutils/share/man" $manpath)
fi

# Put personal bin directory before all others
path=("$HOME/bin" $path)

# Load plugins
source $XDG_CONFIG_HOME/zsh/plugins/directory-history.zsh

# Add fuzzy finding support for completions, search, etc
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Initialize ASDF
if [[ -f ~/.asdf/asdf.sh ]]; then
    source ~/.asdf/asdf.sh
fi

# Initialize completions
autoload -U compinit
compinit

# Load kubectl completions
[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f $XDG_CONFIG_HOME/zsh/.p10k.zsh ]] || source $XDG_CONFIG_HOME/zsh/.p10k.zsh

# Load theme
source $XDG_DATA_HOME/powerlevel10k/powerlevel10k.zsh-theme

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Homebrew
typeset -gx HOMEBREW_NO_ANALYTICS=1
if [[ -f "$XDG_CONFIG_HOME/brew/API_TOKEN" ]]; then
    typeset -gx HOMEBREW_GITHUB_API_TOKEN="$(cat "$XDG_CONFIG_HOME/brew/API_TOKEN")"
fi

# Start gpg-agent if it's not running
if [ -z "$GPG_TTY" ]; then
    typeset -gx GPG_TTY="$(tty)"
fi

if ! pgrep gpg-agent >/dev/null; then
    unset SSH_AUTH_SOCK
    typeset -gx SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    gpgconf --launch gpg-agent
fi

# When available, make use of ccache
if type -p ccache >/dev/null; then
    typeset -gx USE_CCACHE=1
fi

# Set common flags for Elixir/Erlang
typeset -gx ELIXIR_EDITOR="vim +__LINE__ __FILE__"
typeset -gx ERL_AFLAGS="-kernel shell_history enabled"

# Set GOPATH
typeset -gx GOPATH=$HOME

# Java
_java=`asdf which java 2>&1 >/dev/null`
if [[ $? -eq 0 ]]; then
    typeset -gx JAVA_HOME=$(asdf where java)
fi
if [[ -z "$JAVA_HOME" ]] && [[ -f /usr/libexec/java_home ]]; then
    if _java_home=`/usr/libexec/java_home 2>&1 >/dev/null`; then
        typeset -gx JAVA_HOME="${_java_home}"
    fi
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    path=($HOME/.cargo/bin $path)
fi

# direnv
if ! type -p _direnv_hook >/dev/null; then
    eval "$(direnv hook zsh)";
fi

# kubebuilder
if [ -d /usr/local/kubebuilder ]; then
    path+=(/usr/local/kubebuilder/bin)
fi

# minikube
typeset -gx MINIKUBE_HOME="${XDG_DATA_HOME}/minikube"
typeset -gx MINIKUBE_WANTUPDATENOTIFICATION=false
if type -p minikube >/dev/null; then
    eval "$(minikube docker-env --shell=zsh)";
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

alias history='fc -l 1'
alias vi='nvim'
alias vim='nvim'
alias ff='find * -type f | fzf > selected'
alias gp='graphite'

_kill_emacs() {
    pgrep emacs | xargs kill
}
alias killemacs='_kill_emacs'

_unfuck_emacs() {
    pgrep emacs | xargs kill -USR2
}
alias unfuck_emacs='_unfuck_emacs'

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
