#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

git pull origin master;

function doIt() {
  rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" \
        -avh --no-perms . ~;
  source ~/.bash_profile;
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
  doIt;
else
  read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1;
  echo "";
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    doIt;
  fi;
fi;
unset doIt;

echo "Your dotfiles have been synchronized!"

read -p "Have you installed the XCode Command Line Tools? (y/n) " -n 1;
echo "";
if [[ $REPLY =~ ^[Nn]$ ]]; then
  echo "Let's do that first.."
  xcode-select --install
fi;

echo ""
read -p "Do you want to bootstrap your homebrew installation? (y/n) " -n 1;
echo "";
if [[ $REPLY =~ ^[Yy]$ ]]; then
  echo "Configuring homebrew..."
  brew bundle `pwd`/Brewfile
fi;

echo ""
echo "All done!"
