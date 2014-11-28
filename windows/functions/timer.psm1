<#
  .synopsis
  A handy wrapper around measure-command
#>

# Use stricter rules for evaluating this script
set-strictmode -version latest | out-null

# Import datetime helpers
import-module (join-path $PSScriptRoot ..\includes\datetime.psm1)

function timer {
<#
#>
  param(
    [parameter(helpmessage='A path to a script to be executed.')]
    [alias('s', 'eval', 'exec')]
    [validatenotnullorempty()]
    $script = $null
  )

  $stopwatch = new-object system.diagnostics.stopwatch
  switch ($script.GetType()) {
    [string] {
      # Check to see if this is a function reference
      # If not, check to see if it's a path to a *.ps1 script
      # If it's a path, but not a valid script, pass the string to output, write a warning to the host
      # If it's not a path either, do the same as above, but with an appropriate warning message
    }
    [scriptblock] {

    }
  }
}