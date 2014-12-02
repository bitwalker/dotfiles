<#
  .SYNOPSIS
  Most of this profile is broken out into modules. See includes
  directory for the interesting stuff.
#>

# Use stricter rules for evaluating this script
set-strictmode -version latest
# Friendlier errors
trap { trace-error $error[0]; return; }
# Ensure that any errors force the trap block to be executed
$ErrorActionPreference = 'Stop'

# Load submodules
$_includes = join-path (resolve-path $PSScriptRoot) includes
import-module (join-path $_includes logging.psm1)
import-module (join-path $_includes helpers.psm1)
import-module (join-path $_includes environment.psm1)
import-module (join-path $_includes file.psm1)
import-module (join-path $_includes crypto.psm1)
import-module (join-path $_includes functional.psm1)
import-module (join-path $_includes installer.psm1)

# Load aliases
. (join-path $PSScriptRoot aliases.ps1)

# Exports
$REPOS            = "C:\Repositories"
$PROFILE_DIR      = split-path $PROFILE
$PS_PROMPT        = resolve-path (join-path $PSScriptRoot prompt.ps1)
$SUBLIME_PACKAGES = "$HOME\Dropbox\Applications\SublimeText\Data\Packages"

# Import prompt
. (join-path $PSScriptRoot prompt.ps1)

# Load posh-git profile
. (join-path $PSScriptRoot 'modules\poshgit\profile.ps1')

# Reset home directory
mkdirp $REPOS
cd     $REPOS

# So fresh and so clean clean
clear
