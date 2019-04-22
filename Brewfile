#!/usr/bin/env bash

set -x

INSTALLED=$(brew list)
TAPPED=$(brew tap)

TAPS=$(cat <<EOM
caskroom/fonts
d12frosted/emacs-plus
neovim/neovim
EOM
)

# Install missing taps
IFS=$'\n'
for tap in $TAPS; do
  if ! echo "$TAPPED" | grep "$tap" >/dev/null; then
    brew tap $tap;
  fi
done
unset IFS

# Update metadata
brew update

# Upgrade already installed packages
brew upgrade

CASKS=$(cat <<EOM
kitty
font-fantasque-sans-mono
EOM
)

# Install casks
IFS=$'\n'
for cask in $CASKS; do
  pkgname=$(echo "$cask" | sed -e 's/ .*//')
  if ! echo "$INSTALLED" | grep "$pkgname" >/dev/null; then
    brew cask install $pkg
  fi
done
unset IFS

# Desired packages
PACKAGES=$(cat <<EOM

# Utilities
coreutils
moreutils
findutils
wget --with-iri
the_silver_searcher
ripgrep
git
tree
dtrx
p7zip
jq
direnv

# Ncurses disk usage utility
ncdu

# Use gnu-sed rather than builtin sed
gnu-sed --with-default-names

# Build tools and common dependencies
autoconf
automake
openssl
unixodbc

# Shells
bash
bash-completion
fish

# Editors
emacs-plus \
    --without-spacemacs-icon \
    --with-natural-title-bar \
    --with-dbus \
    --with-24bit-color

neovim

# Languages
python
python3

# Tmux
tmux
reattach-to-user-namespace

EOM
)
PACKAGES=$(echo "$PACKAGES" | sed -e '/#.*$/d' -e '/^$/d')

IFS=$'\n'
for pkg in $PACKAGES; do
  pkgname=$(echo "$pkg" | sed -e 's/ .*//')
  if ! echo "$INSTALLED" | grep "$pkgname" >/dev/null; then
    brew install $pkg
  fi
done
unset IFS

# Cleanup post-install
brew cleanup
