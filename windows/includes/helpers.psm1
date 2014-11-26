<#
  .SYNOPSIS
  General helper functions that don't fit much of anywhere else
#>

set-strictmode -version latest

$_includes = $PSScriptRoot
import-module (join-path $_includes logging.psm1)
import-module (join-path $_includes functional.psm1)

function show-less {
<#
  .SYNOPSIS
  A pager, like less, for powershell. Of course, if you have less
  installed, I'd recommend using that first
  .INPUTS
  Any input you'd normally send to more.com, out-host, or less
  .OUTPUTS
  The input data, but paged
#>
  param(
    [parameter(ValueFromPipeline=$true)]$input
  )
  $input | out-host -paging
}

function get-help-gui([string]$topic) {
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