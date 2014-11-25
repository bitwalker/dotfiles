<#
  .SYNOPSIS
  General helper functions that don't fit much of anywhere else
#>

set-strictmode -version latest

import-module .\logging.psm1

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