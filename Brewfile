# Make sure we’re using the latest Homebrew
update

# Upgrade any already-installed formulae
upgrade

# Install build tools
install autoconf
install automake

# Install GNU core utilities (those that come with OS X are outdated)
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
install coreutils

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
install findutils

# Install Shells
install bash
install bash-completion
install fish

# Install wget with IRI support
install wget --enable-iri

# Install more recent versions of some OS X tools
install vim --override-system-vi
install apple-gcc42

# Install Runtimes
install erlang
install scala
install sbt
install python
install ruby
install node

# Install other useful utilities
install ack
install git
install tree
install gnu-sed
install todo-txt

# Install Android SDK
install android-sdk

# Install brew-cask
tap phinze/cask
install brew-cask

# Remove outdated versions from the cellar
cleanup
