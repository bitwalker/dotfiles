# Common variables
$REPOS            = "C:\Repositories"
$SUBLIME_PACKAGES = "C:\Users\Paul\AppData\Roaming\Sublime Text 3\Packages"

# Load aliases
. .\aliases.ps1
# Load helper functions
. .\functions.ps1

# Now that the helpers have been loaded, trap any future errors
# and write them using my own error formatter
trap { handle-error $error[0] }

# Load the bootstrapper
. .\bootstrap.ps1

# Ensure all my shell environment dependencies are in place
$dependencies = @{
  "chocolatey" = "require_chocolatey";
  "psake"      = "require_psake";
  "git"        = "require_git";
  "vim"        = "require_vim";
  "dotfiles"   = "require_dotfiles";
  "sublime"    = "require_sublime";
}
require-all $dependencies

# Load my posh-git profile
. .\poshgit\profile.ps1

# Reset home directory and shell
mkdirp $REPOS
cd $REPOS
clear
