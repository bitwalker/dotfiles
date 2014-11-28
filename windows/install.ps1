<#
  .SYNOPSIS
  Handles initial bootstrapping of profile on a new system.
  This installer script is idempotent, so feel free to run it
  any time you want to make sure your environment is synced with
  the latest version in source control.

  .NOTES
  **IMPORTANT**
  Make sure you add all custom scripts in .\includes\custom. They
  will be auto imported on shell start, and will not be overwritten by
  this installer. Anything else on the level of this folder and down could
  be overwritten at any time!
#>

# Use stricter rules for evaluating this script
set-strictmode -version latest | out-null


write-host 'Bootstrapping installer...'
# We require an elevated shell for some portions of the installation process,
# so fail early if we don't already have one.
assert-isElevatedShell | out-null
# Include some of the extension modules for use during installation
$_includes = resolve-path (join-path $PSScriptRoot includes)
import-module (join-path $_includes helpers.psm1) | out-null
import-module (join-path $_includes file.psm1) | out-null
import-module (join-path $_includes logging.psm1) | out-null
import-module (join-path $_includes installer.psm1)
import-module (join-path $_includes git.psm1)
# Trap all errors and handle them to produce friendlier, more useful output
trap { trace-error $error[0]; return; }
# Ensure that any errors force the trap block to be executed
$ErrorActionPreference = 'Stop'
# Since these scripts are not locally created, nor signed (yet),
# we need to ensure the execution policy is set to unrestricted
grant-unrestrictedExecution
# Load dependency manifest
$json         = get-content (resolve-path .\dependencies.json) -encoding utf8
$dependencies = convertfrom-json $json

show-success 'Dependency manifest loaded!'
# Run sync-depedencies, which will check to make sure all
# deps are installed, and any applicable preferences are symlinked
sync-dependencies $dependencies
# Link profile module to $HOME\Documents\WindowsPowerShell directory
$from_path = $PSScriptRoot
$to_path   = split-path $PROFILE
$files = (ls .\* -include *.ps1,*.psm1 -exlude install.ps1 -recurse) | % { get-relativepath $_ }
# Change to the profile path to make this linking stuff a bit cleaner
pushd $to_path
# Link all these files to the source repo so that updates are automatically propogated from now on
$files | foreach {
  new-directory (split-path $_)
  $file = (join-path $from_path $_)
  if (!(test-path $file)) {
    # Only symlink if there isn't already a file there
    new-link -s $_ $file
  }
}
# Change back to the original directory
popd
show-success "Profile module $version has been installed succesfully!"
