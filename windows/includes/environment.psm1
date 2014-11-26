<#
  .SYNOPSIS
  Contains helpers for inspecting the current environment, such as
  locating the source of a given command or executable in $PATH, getting
  environment variables, etc.
#>

set-strictmode -version latest

$_includes = $PSScriptRoot
import-module (join-path $_includes functional.psm1)

function find-which($query) {
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

function get-env($query) {
  ls env: | where { $_.Name -match $query }
}

function get-psql-version {
  $postgres = pwhich "PostgreSQL" | select-n -count 1
  $postgres | where { $_.name -match "\d{1,2}\.\d{1,2}" } | % { $_.Name }
}