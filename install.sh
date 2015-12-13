#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

function doIt() {
  # sync all dotfiles from this directory
  rsync --exclude ".git/" --exclude ".DS_Store" --exclude "install.sh" \
        --exclude "init" --exclude "Brewfile" \
        -avdh --no-perms . ~;
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

read -p "Install XCode Command Line Tools? (y/n) " -n 1;
echo "";
if [[ $REPLY =~ ^[Yy]$ ]]; then
  echo "Running installer.."
  xcode-select --install
fi;

echo ""
read -p "Install Homebrew packages? (y/n) " -n 1;
echo "";
if [[ $REPLY =~ ^[Yy]$ ]]; then
  echo "Running homebrew..."
  brew bundle Brewfile
fi;

echo ""
echo "All done!"
