#!/usr/bin/env bash
brew tap homebrew/devel-only
brew tap railwaycat/emacsmacport

# Make sure we’re using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade

# Install GNU core utilities (those that come with OS X are outdated)
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
brew install coreutils

# Install some other useful utilities like `sponge`.
brew install moreutils

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils

# Install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed --with-default-names

# Install build tools
brew install autoconf
brew install automake

# Install common build dependencies
brew install openssl
brew install libyaml
brew install readline
brew install libxslt
brew install libtool
brew install unixodbc

# Install Shells
brew install bash
brew install bash-completion
brew install fish

# Install wget with IRI support
brew install wget --with-iri

# Install more recent versions of some OS X tools
brew install emacs-mac --with-spacemacs-icon
brew install vim --override-system-vi
brew install apple-gcc42

# Install other useful utilities
brew install the_silver_searcher
brew install git
brew install tree
brew install dtrx p7zip
brew install jq
brew install direnv

# Install Runtimes
brew install erlang
brew install --devel homebrew/devel-only/rebar3
brew install python python3
brew install rbenv ruby-build

brew linkapps

# Remove outdated versions from the cellar
brew cleanup
