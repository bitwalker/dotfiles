#!/bin/bash

function print_message {
    echo
    for msg in "$@"; do
        echo "$msg"
    done
    echo
}

print_message "Updating submodules..."
git pull --recurse-submodules && git submodule update --recursive --init


print_message 'Linking dotfiles and directories..'
for item in .*
do
    if [ $item != '.' ] && [ $item != '..' ]; then
        echo "Linking $item to $HOME/$item"
        ln -sf `pwd`/$item $HOME/
    fi
done

# Update running environment
print_message 'Updating environment...' \
    'You can ignore the "bind: warning: line editing not enabled"' \
    'Simply run "source ~/.bash_profile" once installation is complete to get those binds!'
source $HOME/.bash_profile

# Build vim plugins
print_message 'Building vim plugins...'
if hash rake 2>/dev/null; then
    cd `pwd`/.vim/bundle/command-t && rake make
else
    echo 'Ruby/Rake not installed. Unable to build command-t plugin'
fi

print_message 'dotfiles installed!' \
    'Now run osx.sh if this is on a Mac, and you're golden!'
