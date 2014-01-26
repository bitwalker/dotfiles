#!/bin/bash

echo "Linking dotfiles and directories.."
for item in .*
do
    if [ $item != '.' ] && [ $item != '..' ] && [ $item != '.git' ] && [ $item != '.gitingore' ]; then
        echo "Linking `pwd`/$item to $HOME/$item"
        ln -sf -t $HOME `pwd`/$item
    fi
done

echo "Loading homebrew preferences..."
brew bundle $HOME/Brewfile

echo "Installing OSX preferences..."
. $HOME/.osx

echo "Success! dotfiles installed!" \
