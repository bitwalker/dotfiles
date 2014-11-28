<#
  .SYNOPSIS
  Contains helpers for inspecting the current environment, such as
  locating the source of a given command or executable in $PATH, getting
  environment variables, etc.
#>

# Ensure errors stop execution so that they can be caught
$ErrorActionPreference = 'Stop'
# Use stricter evaluation for this module
set-strictmode -version latest
# Load included dependencies
$_includes = $PSScriptRoot
import-module (join-path $_includes functional.psm1)

function Find-Which($query) {
<#
  .SYNOPSIS
  Locate where a particular command or executable is located
  .PARAMETER query
  The search term to use
#>
  $programs      = gci -path 'env:programfiles' | % { $_.value }
  $programs_x86  = gci -path 'env:programfiles(x86)' | % { $_.Value }
  $program_paths = ls $programs $programs_x86 `
    -recurse -filter '*.exe' | `
    select-object DirectoryName | `
    sort-object -unique
  $env           = $env:path
  $env:path      = "$env;"
  $matches = get-command $query
  $paths = @($programs, $programs_x86) + ($env:path.split(';'))
  ls -Path $paths -Recurse -File `
     -Attributes !System,!Offline,!Temporary,!Hidden `
     -Filter '*.exe *.bat *.ps1 *.psm1'
}

function Get-ServicesFuzzy {
<#
  .SYNOPSIS
  Query the status of all services matching  the provided query and flags.
  .PARAMETER service
  The query to search for services by (uses `-match`)
  .PARAMETER sort
  The column to sort by. Valid values are status, name, and displayname
  .PARAMETER ascending
  This switch determines whether the sort is ascending order or not. Defaults to true.
  .PARAMETER running
  This switch will include results for currently running services. Defaults to false.
  .PARAMETER stopped
  This switch will include results for currently stopped services. Defaults to false.
  .PARAMETER all
  This switch will include all results, regardless of status. Defaults to true.
#>
  param(
    [parameter(mandatory=$true, valueFromPipeline=$true)]
    [validateNotNull()]
    [string] $service,
    [validateNotNull()]
    [string] $sort      = 'name',
    [alias('asc')]
    [switch] $ascending = $true
    [alias('r')]
    [switch] $running   = $false,
    [alias('s')]
    [switch] $stopped   = $false,
    [alias('rs', 'a', 'any')]
    [switch] $all       = $true,
  )
  $services = get-service | where { $_.name -match $service }
  $services = if ($all -or ($running -and $stopped)) { $services }
              elseif ($running) { $services | where { $_.status -eq 'running' }}
              elseif ($stopped) { $services | where { $_.status -eq 'stopped' }}
              else              { $services }

  if ($asc) { $services | sort $sort }
  else      { $services | sort $sort -descending }
}

function Get-Env([string]$query) {
<#
  .SYNOPSIS
  Query the current environment for a variable which
  matches the provided query. The query uses `-match`
  internally, so any valid syntax for that flag will
  be used during the search.
  .PARAMETER query
  The search term to use
#>
  ls env: | where { $_.Name -match $query }
}

function Get-PsqlVersion {
<#
  .SYNOPSIS
  Handy little helper for finding the current version of
  PostgreSQL, if you have it installed.
#>
  $postgres = pwhich "PostgreSQL" | select-n -count 1
  $postgres | where { $_.name -match "\d{1,2}\.\d{1,2}" } | % { $_.Name }
}