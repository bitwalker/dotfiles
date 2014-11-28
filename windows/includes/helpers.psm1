<#
  .synopsis
  General helper functions that don't fit much of anywhere else
#>

# Ensure errors stop execution so that they can be caught
$errorActionPreference = 'stop'
# Use stricter evaluation for this module
set-strictmode -version latest

$_includes = $PSScriptRoot
import-module (join-path $_includes logging.psm1)
import-module (join-path $_includes functional.psm1)

function show-help([string]$topic) {
<#
  .synopsis
  Helper for displaying full help for a topic in a GUI window.
  .parameter topic
  The help topic to open, or search for
#>
  if (isEmpty($topic))
    { show-error "Must provide a topic name."; break; }
  trap
    { show-error "Unable to find topic."; $error.clear(); return; }

  get-help -showwindow $topic
}

function assert-isElevatedShell {
<#
  .SYNOPSIS
  Asserts that the current user is running as an elevated user.
  If the assertion fails, an exception is thrown and returned to the user.
#>
  $identity  = [Security.Principal.WindowsIdentity]::GetCurrent()
  $principal = [Security.Principal.WindowsPrincipal] $identity
  $isAdmin   = $principal.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")

  if (-not $isAdmin) { throw "Assertion failed: shell is not running as an elevated user!" }
}

function grant-unrestrictedExecution {
<#
  .SYNOPSIS
  Sets the execution policy to unrestricted, permanently.
  .NOTES
  Requires an elevated shell to run, or will throw.
#>
  assert-isElevatedShell
  if ((get-executionpolicy) -ne 'unrestricted') {
    set-executionpolicy unrestricted -force
    show-warning 'Execution policy set to unrestricted.'
  }
}

function stop-frozenProcesses([switch]$force = $false) {
<#
  .synopsis
  Finds all non-responsive processes and prompts to kill each one individually.
  If -force is given, then no prompt is shown, and the kill signal is sent to all
  tasks reporting as non-responsive.
  .parameter force
  Forces all non-responsive processes to kill
#>
  get-process | `
      where   { -not $_.responding } | `
      foreach {
        $message = "$($_.displayname) is not responding, would you like to kill?"
        $kill    = $_.kill
        $confirm = read-host $message

        if ($confirm -match 'y') {
          try {
            $kill.Invoke()
            write-host "$($_.displayname) has been killed!" -foregroundcolor green
          } catch {
            $ex = $error[0].Exception.Message
            write-host "$($_.displayname) could not be killed! $ex" -foregroundcolor -red
          }
        } else {
          write-host "Skipping $($_.displayname).." -foregroundcolor yellow
        }
      }
}