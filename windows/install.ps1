$identity  = [Security.Principal.WindowsIdentity]::GetCurrent()
$principal = [Security.Principal.WindowsPrincipal] $identity
$isAdmin   = $principal.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")

import-module .\includes\file.psm1
import-module .\includes\logging.psm1
import-module .\includes\installer.psm1

# Friendlier errors
trap { trace-error $error[0]; return; }

# Just make things easier by requiring admin
if (!$isAdmin) {
  show-error 'You need to be admin to run this installer.'
  return
}

# This can always be changed back later, but for now, fuck it
if ((get-executionpolicy) -ne 'unrestricted') {
  set-executionpolicy unrestricted -force
  show-warning 'Execution policy set to unrestricted.'
}

# Load dependency manifest
$json         = get-content (resolve-path .\dependencies.json) -encoding utf8
$dependencies = convertfrom-json $json

show-success 'Dependency manifest loaded!'

# Run sync-depedencies, which will check to make sure all
# deps are installed, and any applicable preferences are symlinked
sync-dependencies $dependencies

# Link profile module to $HOME\Documents\WindowsPowerShell directory
$from_path = resolve-path $PSScriptRoot
$to_path   = split-path $PROFILE
$files = (ls .\* -include *.ps1,*.psm1 -exlude install.ps1 -recurse) | % { get-relativepath $_ }

pushd $to_path

$files | foreach {
  new-directory (split-path $_)
  $file = (join-path $from_path $_)
  if (!(test-path $file)) {
    # Only symlink if there isn't already a file there
    new-link -s $_ $file
  }
}
