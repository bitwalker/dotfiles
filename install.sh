#!/usr/bin/env bash

set -e

cd "$(dirname "${BASH_SOURCE}")"

CWD=$(pwd)

printf "# Syncing to home folder...\n"

mkdir -p $HOME/.terminfo
tic -x -o $HOME/.terminfo $CWD/terminfo-24bit.src

function syncFile() {
    local sourceFile="$1"
    ln -sf "$CWD/${sourceFile}" "$HOME/${sourceFile}"
}

function doSync() {
    syncFile ".aliases"
    syncFile ".bash_profile"
    syncFile ".bash_prompt"
    syncFile ".bashrc"
    syncFile ".config"
    syncFile ".exports"
    syncFile ".functions"
    syncFile ".gdbinit"
    syncFile ".gemrc"
    syncFile ".gitconfig"
    syncFile ".gitmessage.txt"
    syncFile ".hushlogin"
    syncFile ".inputrc"
    syncFile ".tmux.conf"
    syncFile ".tmux-osx.conf"
    syncFile ".vimrc"
    syncFile ".wgetrc"
    syncFile ".zshenv"
    syncFile "bin"
    syncFile "git"
    ln -sf ~/.vimrc ~/.config/nvim/init.vim
    mkdir -p ~/.config/.vim
    ln -sf ~/.vimrc ~/.config/.vim/init.vim
    ln -sf ~/.config/hammerspoon ~/.hammerspoon
    return 0
}
if [ "$1" == "--force" -o "$1" == "-f" ]; then
	doSync
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]; then
	    doSync
    else
        printf "Skipped.\n\n"
	fi
fi
unset doSync

# Vim package management
if [ ! -d ~/.config/nvim/pack/minpac/opt/minpac ]; then
    printf "Installing minpac for vim package management.."
    mkdir -p ~/.config/nvim/pack/minpac/opt
    git clone https://github.com/k-takata/minpac.git ~/.config/nvim/pack/minpac/opt/minpac
    printf "done!\n"
fi

if [ ! -d ~/.config/emacs ]; then
    printf "Installing Emacs config.."
    git clone git@github.com:bitwalker/doom-emacs ~/.config/emacs
    pushd ~/.config/emacs
    git checkout develop
    git remote add hlissner git@github.com:hlissner/doom-emacs.git
    popd
    printf "done!\n"
fi

echo "dotfiles have been synchronized!"

# Install tmux plugins
read -p "Install tmux plugins? (y/n) " -n 1;
echo "";
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Installing tmux plugins.."
    sudo pip3 install powerline-status
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi;

# Homebrew
if which brew >/dev/null; then
    read -p "Install Homebrew packages? (y/n) " -n 1
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        brew bundle --file "$CWD/Brewfile"
    fi
else
    printf "\nHomebrew not installed! Skipping package installation..\n\n"
fi

# Emacs
if which emacs >/dev/null; then
    echo "Compiling Emacs configuration.."
    pushd ~/.config/emacs
    if ! bin/doom sync; then
        echo "ERROR: Issue installing emacs, continuing with rest of installation, but make sure you revisit this after install"
    else
        echo "Emacs is ready!"
    fi
    popd
else
    echo "Skipping Emacs configuration, 'which emacs' failed with non-zero exit"
fi

# ASDF
read -p "Install asdf version manager? (y/n) " -n 1;
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [ ! -d ~/.asdf ]; then
        git clone https://github.com/asdf-vm/asdf.git ~/.asdf
    else
        pushd ~/.asdf
        git fetch origin
        popd
    fi
    # Update to latest version
    pushd ~/.asdf
    __latest_asdf=$(git tag -l | tail -n 1)
    git checkout $__latest_asdf
    popd
    source ~/.asdf/asdf.sh
    echo "Adding language plugins to asdf.."
    asdf plugin-add erlang
    asdf plugin-add elixir
    asdf plugin-add nodejs
    #asdf plugin-add golang https://github.com/kennyp/asdf-golang.git
fi

echo ""
echo "All done!"
