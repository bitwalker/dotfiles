#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

CWD=$(pwd)

function doIt() {
  find . \
       -maxdepth 1 \
         \! \( -name ".git" \
               -or -name "emacs" \
               -or -name "install.sh" \
               -or -name "." \
               -or -name ".#*" \) \
        -execdir ln -sf "$CWD/{}" "$HOME/{}" \;
  sudo cp emacs /usr/local/bin/
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

read -p "Install Homebrew packages? (y/n) " -n 1;
echo "";
if [[ $REPLY =~ ^[Yy]$ ]]; then
  sudo xcodebuild -license
  echo "Running homebrew..."
  . Brewfile
fi;

echo "Fetching iTerm2 color schemes..."
git clone https://github.com/mbadolato/iTerm2-Color-Schemes ~/src/iTerm2-Color-Schemes

echo "Installing Node.js version manager..."
curl -L http://git.io/n-install | bash

echo "Installing latest stable Node.js version..."
n stable

echo "Installing Elixir version manager..."
git clone https://github.com/mururu/exenv.git ~/.exenv
echo 'export PATH="$HOME/.exenv/bin:$PATH"' >> ~/.bash_profile

echo "Installing Elixir..."
git clone https://github.com/mururu/elixir-build.git ~/.exenv/plugins/elixir-build
exenv install 1.2.0

echo ""
echo "All done!"
