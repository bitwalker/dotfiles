<#
  .synopsis
  Contains individual installation tasks for the installer script
#>

$_includes = resolve-path .
import-module (join-path $_includes logging.psm1)
import-module (join-path $_includes file.psm1)
import-module (join-path $_includes git.psm1)

function exec {
<#
  .synopsis
  Wraps execution of installer tasks to provide easier
  control over verbose output, and standardized task reporting
#>
  param(
    [parameter(position=0)]
    [validatenotnull()]
    [string]
    $description,
    [parameter(position=1)]
    [switch]
    $verbose = $false,
    [parameter(valueFromPipeline=$true, position=2)]
    [validatenotnull()]
    [scriptblock]
    $command
  )

  show-info "Task: $message"

  if ($verbose) { & $command }
  else          { & $command | out-null }
}

function sync-profile {
<#
  .synopsis
  Syncs files and directories from this project to
  the $HOME\Documents\WindowsPowerShell directory
#>
  $from_path = join-path $PSScriptRoot ..
  $to_path   = split-path $PROFILE

  sync-files $from_path $to_path `
    -verbose $true `               # Use verbose logging and let exec ignore it if desired
    -xf install.ps1,installer.psm1 # Exclude the installer
}

function restore-dependencies {
<#
  .synopsis
  Installs core dependencies (chocolatey, psake, gnuwin, etc.)
#>
  # Get paths
  $installer_manifest_path     = resolve-path ..\.manifest
  $dependencies_manifest_path  = resolve-path ..\dependencies.json
  # Load manifests
  $dependencies_json           = get-content $dependencies_manifest_path -encoding utf8
  $dependencies                = convertfrom-json $json
  # Determine required installations
  # Install packages in order
}

function install-cmdrc {
<#
  .synopsis
  Adds AutoRun registry key which executes .cmdrc on cmd.exe startup
#>
  set-itemproperty 'hkcu:\Software\Microsoft\Command Processor' `
      -name AutoRun `
      -value 'call %USERPROFILE%/.cmdrc'
}

function install-ansicon {
<#
  .synopsis
  Runs ansicon.exe installer to add support for ANSI color codes in cmd.exe
#>
  $ansicon = resolve-path ..\extensions\ansicon.exe
  & $ansicon -I
}

function update-profileVersion {
<#
  .synopsis
  Build version information from git and write it to VERSION file
  located in profile root directory, for reference
  .outputs
  Current version as a string
#>

}