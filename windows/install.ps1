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

param(
  [parameter(HelpMessage='Allow output from individual commands to be displayed during installation')]
  [switch]
  $verbose = $false
)

# Stop executing if an error is encountered
$ErrorActionPreference = 'Stop'

# Use stricter rules for evaluating this script
set-strictmode -version latest | out-null

write-host 'Bootstrapping installer...'

# We require an elevated shell for some portions of the installation process,
# so fail early if we don't already have one.
assert-isElevatedShell

# Include some of the extension modules for use during installation
$_includes = resolve-path (join-path $PSScriptRoot includes)
import-module (join-path $_includes helpers.psm1)
import-module (join-path $_includes file.psm1)
import-module (join-path $_includes logging.psm1)
import-module (join-path $_includes installer.psm1) -disableNameChecking

# Trap all errors and handle them to produce friendlier, more useful output
trap {
  trace-error $error[0]
  show-error 'Failed to install profile!'
  break
}

# Link profile module to $HOME\Documents\WindowsPowerShell directory
exec 'Syncing profile scripts...' `
  -verbose $verbose `
  ${function:sync-profile}

# Make sure .cmdrc is setup
show-info 'Installing cmd.exe profile...'
exec 'Installing cmd.exe profile' `
  -verbose $verbose `
  ${function:install-cmdrc}

# Make sure ansicon is installed (adds entry to HKLM:\Software\Microsoft\Command Processor:AutoRun)
show-info 'Installing ansicon...'
exec 'Installing ansicon...' `
  -verbose $verbose `
  ${function:install-ansicon}

# Restore dependencies
exec 'Restoring dependencies...' `
  -verbose $verbose `
  ${function:restore-dependencies}

# Build version information from git and write it to VERSION
# file located in profile root directory, for reference
$version = update-profileVersion

show-success "Profile module $version has been installed succesfully!"
