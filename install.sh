#!/usr/bin/env bash

set -e

cd "$(dirname "${BASH_SOURCE}")"

CWD=$(pwd)

# Create prereqs
mkdir -p ~/bin
mkdir -p ~/.config/nvim
mkdir -p ~/src/github.com/bitwalker

printf "# Syncing to home folder...\n"
function doSync() {
    find . \
        -maxdepth 1 \
        \! \( -name ".git" \
              -or -name ".DS_Store" \
              -or -name "emacs" \
              -or -name "init" \
              -or -name "install.sh" \
              -or -name "Brewfile" \
              -or -name "*.txt" \
              -or -name "*.md" \
              -or -name "." \
              -or -name ".#*" \) \
              -execdir ln -sf "$CWD/{}" "$HOME/{}" \;
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
        printf "Aborted.\n\n"
        exit 0
	fi
fi
unset doSync

. Brewfile

if [ ! -d ~/.emacs.d ]; then
    printf "Installing Emacs config.."
    git clone git@github.com:bitwalker/doom-emacs ~/.emacs.d
    pushd ~/.emacs.d
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
    sudo pip install powerline-status
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi;

# XCode
if which xcode-select >/dev/null; then
    read -p "Install XCode Command Line Tools? (y/n) " -n 1
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "Running installer.."
        xcode-select --install
    fi
fi

# Homebrew
if which brew >/dev/null; then
    read -p "Install Homebrew packages? (y/n) " -n 1
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        . $CWD/Brewfile
    fi
else
    printf "\nHomebrew not installed! Skipping package installation..\n\n"
fi

# Emacs
if which emacs >/dev/null; then
    echo "Compiling Emacs configuration.."
    pushd ~/.emacs.d
    make
    popd
    echo "Emacs is ready!"
else
    echo "Skipping Emacs configuration, 'which emacs' failed with non-zero exit"
fi

# iTerm
read -p "Install iTerm2 preferences? (y/n) " -n 1;
echo "";
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [ ! -d ~/src/iTerm2-Color-Schemes ]; then
        git clone https://github.com/mbadolato/iTerm2-Color-Schemes ~/src/iTerm2-Color-Schemes
    else
        pushd ~/src/iTerm2-Color-Schemes
        git pull origin master
        popd
    fi
    open ~/src/iTerm2-Color-Schemes/schemes/Glacier.itermcolors
    open ~/src/iTerm2-Color-Schemes/schemes/Hybrid.itermcolors
    open ~/src/iTerm2-Color-Schemes/schemes/Hardcore.itermcolors
    if which defaults >/dev/null; then
        defaults read $CWD/com.googlecode.iterm2.plist
    fi
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
    echo "Adding Erlang, Elixir, Go plugins"
    asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git
    asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git
    asdf plugin-add golang https://github.com/kennyp/asdf-golang.git
fi

echo "Installing Python-based utilities.."
pip install -r $CWD/requirements.txt

# Update shell environment based on profile
source ~/.bash_profile

echo ""
echo "All done!"
