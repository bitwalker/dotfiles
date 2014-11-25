<#
  .SYNOPSIS
  Most of this profile is broken out into modules. See includes
  directory for the interesting stuff.
#>

# Enforce strict mode
set-strictmode -version latest

# Load submodules
import-module .\includes\logging.psm1
import-module .\includes\helpers.psm1
import-module .\includes\environment.psm1
import-module .\includes\file.psm1
import-module .\includes\crypto.psm1
import-module .\inculdes\functional.psm1
import-module .\includes\installer.psm1


# Load aliases
. .\aliases.ps1

# Now that any dependencies have been loaded, trap all future errors
# and write them using my own error formatter
trap { trace-error $error[0] }

# Exports
$REPOS            = "C:\Repositories"
$PROFILE_DIR      = split-path $PROFILE
$PS_PROMPT        = resolve-path .\prompt.ps1
$SUBLIME_PACKAGES = "$pgfiles\SublimeText\Data\Packages"

# Import prompt
. .\prompt.ps1

# Load posh-git profile
. .\poshgit\profile.ps1

# Reset home directory
mkdirp $REPOS
cd     $REPOS

# So fresh and so clean clean
clear