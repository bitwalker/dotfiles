tap homebrew/devel-only
tap railwaycat/emacsmacport

# Make sure we’re using the latest Homebrew
update

# Upgrade any already-installed formulae
upgrade

# Install GNU core utilities (those that come with OS X are outdated)
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
install coreutils

# Install some other useful utilities like `sponge`.
install moreutils

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
install findutils

# Install GNU `sed`, overwriting the built-in `sed`.
install gnu-sed --with-default-names

# Install build tools
install autoconf
install automake

# Install Shells
install bash
install bash-completion
install fish

# Install wget with IRI support
install wget --with-iri

# Install more recent versions of some OS X tools
install emacs-mac
install vim --override-system-vi
install apple-gcc42

# Install other useful utilities
install the_silver_searcher
install git
install tree
install dtrx
install jq

# Install Runtimes
install erlang rebar3
install python python3
install ruby
install node

# Install brew-cask
tap phinze/cask
install brew-cask

# Remove outdated versions from the cellar
cleanup
