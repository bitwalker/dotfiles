#!/bin/bash

echo "Linking dotfiles and directories.."
for item in .*
do
    if [ $item != '.' ] && [ $item != '..' ] && [ $item != '.git' ] && [ $item != '.gitingore' ]; then
        echo "Linking `pwd`/$item to $HOME/$item"
        ln -sf -t $HOME `pwd`/$item
    fi
done

echo "dotfiles installed!" \
    "Now run osx.sh if this is on a Mac, and you're golden!"
