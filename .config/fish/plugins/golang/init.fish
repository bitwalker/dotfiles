# Ensure GOPATH is defined
set -gx GOPATH $HOME

# Make sure install directory is created and on the PATH
mkdir -p "$GOPATH/bin"
set PATH "$GOPATH/bin" $PATH
