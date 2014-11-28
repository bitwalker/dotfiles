<#
  .SYNOPSIS
  Helpers for handling files, directories, and paths
#>

# Ensure errors stop execution so that they can be caught
$ErrorActionPreference = 'Stop'
# Use stricter evaluation for this module
set-strictmode -version latest
# Load module dependencies
$_includes = $PSScriptRoot
import-module (join-path $_includes logging.psm1)
import-module (join-path $_includes functional.psm1)

function test-directory([string]$path) {
<#
  .SYNOPSIS
  Determines if a directory exists.
  .PARAMETER path
  The path to the directory in question.
  .OUTPUTS
  Boolean
#>
  trap { $false; return; }
  if (test-path $path) {
    [System.IO.Directory]::Exists(resolve-path $path)
  } else {
    $false
  }
}

function get-relativepath([string]$from = "", [string]$to = "") {
<#
  .SYNOPSIS
  Determine the relative difference between two paths.
  .DESCRIPTION
  Determines the relative difference between two paths, such
  that the result describes how to move from $from, to $to.
  The result of calling this function is rooted at $from.
  .PARAMETER from
  Optional. The location from which the relative path is rooted.
  If no value is provided, the current directory is assumed.
  .PARAMETER to
  Required. The target when resolving the relative path
  .EXAMPLE
  > pwd
  C:\Program Files
  > get-relativepath $HOME
  ..\Users\Paul
  .EXAMPLE
  > get-relativepath 'C:\Users\Paul' 'C:\Program Files'
  ..\..\Program Files
  .EXAMPLE
  > pwd
  C:\Program Files
  > get-relativepath . $HOME
  ..\Users\Paul
#>
  if ($to -eq "" -and $from -ne "") {
    $to   = $from
    $from = "."
  }

  $source = resolve-path $from
  $target = resolve-path $to

  $stack = (get-date | % { $_.Ticks })
  pushd $source -stackname $stack
  $relative = resolve-path -relative $target
  popd -stackname $stack
  return $relative
}

function new-path($path = ${ throw "Requires a valid path!" }) {
<#
  .SYNOPSIS
  Silently creates a directory if it doesn't exist,
  creating paths along the way as necessary. If it does
  exist, it still behaves as though it did.
  .PARAMETER path
  Required. The path to create
  .EXAMPLE
  > new-path C:\foo\bar\baz
#>
  trap { trace-error $error[0]; return; }
  mkdir $path -ErrorAction SilentlyContinue | out-null
}

function remove-all {
<#
  .SYNOPSIS
  A better remove-item/rm command which behaves more like *NIX rm.
  .PARAMETER paths
  One or more paths to remove
  .PARAMETER force
  Force removal of the file or directory
  .PARAMETER recurse
  Recursively deletes files if one of the paths is a directory
  .PARAMETER forceRecurse
  Combines -force and -recurse into one switch.
  .NOTES
  Aliases: rmrf, rmf
  .EXAMPLE
  > remove-all foo.txt
  .EXAMPLE
  > rmf foo.txt
  .EXAMPLE
  > rmrf .\foo
  .EXAMPLE
  > remove-all -rf .\foo
#>
  param(
    [parameter(
      Position=0,
      ValueFromPipeline=$true,
      ValueFromPipelineByPropertyName=$true
    )]
    [string[]]$paths = @(),
    [alias('f')]
    [switch]$force = $false,
    [alias('r')]
    [switch]$recurse = $false,
    [alias('fr', 'rf')]
    [switch]$forceRecurse = $false
  )
  trap { trace-error $error[0]; return; }

  # Setting -fr/-rf sets $force and $recurse as well
  if ($forceRecurse) { $force = $true; $recurse = $true }

  # Lookup table for flags
  $flagmap = @{
    "$true|$true"   = '-force -recurse';
    "$true|$false"  = '-force';
    "$false|$true"  = '-recurse';
    "$false|$false" = ''
  }

  # Check the invocation to see if the alias implies flags
  $command = $MyInvocation.InvocationName
  switch ($command) {
    "rmf"  { $force = $true; }
    "rmrf" { $force = $true; $recurse = $true }
  }
  $flags = $flagmap["$force|$recurse"]
  $expression = 'remove-item {0} {1} -ErrorAction SilentlyContinue'

  $paths | foreach {
    invoke-expression ($expression -f $flags, $_) | out-null
  }
}

function new-link {
<#
  .SYNOPSIS
  Mimic POSIX ln, with some additional extensions
  .EXAMPLE
  > new-link -s C:\path\to\symlink C:\path\to\actual\file
  .NOTES
  Flags:
    -s | -symbolic    Make a symbolic link instead of a hard link
    -a | -all         Link all children. If $to is a a directory, all children
                      of that directory will be linked to corresponding files in $from.
                      When -all is provided, both $from and $to must be directories.
#>
  param(
    [string]$from = ${ throw "Must provide a valid destination path" },
    [string]$to   = ${ throw "Must provide a valid file or directory path" },
    [alias('s')][switch]$symbolic,
    [alias('a')][switch]$all,
    [alias('x')][string[]]$exclude
  )

  $from = resolve-path $from
  $to   = resolve-path $to

  if ($all) { # Link all children

    if (!(test-directory $from)) { throw "Invalid params for -all: `$from is not a directory!" }
    if (!(test-directory $to))   { throw "Invalid params for -all: `$to is not a directory!" }

    $files = @()
    if ($exclude) { $files = (ls $to -exclude $exclude) }
    else          { $files = (ls $to) }

    foreach ($file in $files) {
      $filename = $file.Name
      $filepath = $file.FullName
      $link     = join-path $from $filename
      # If this item is a directory, just link the directory
      if ($file.Attributes.HasFlag([System.IO.FileAttributes]::Directory)) {
        if ($symbolic) { cmd /c mklink /D $link $filepath | out-null }
        else           { cmd /c mklink /D /H $link $filepath | out-null }
      }
      # Must be a file then
      else {
        if ($symbolic) { cmd /c mklink $link $filepath | out-null }
        else           { cmd /c mklink /H $link $filepath | out-null }
      }

      if (!$?) {
        throw "Failed to link $filepath"
      }
    }

    show-success "Links for all matching files in $to have been placed in $from"

  } else {

    $fi = new-object System.IO.FileInfo($to)
    if (!(test-path $to)) { throw "The path $to does not exist!" }

    # Check to see if we're linking a directory for a file
    if ($fi.Attributes.HasFlag([System.IO.FileAttributes]::Directory)) {
      if ($symbolic) { cmd /c mklink /D $from $to | out-null }
      else           { cmd /c mklink /D /H $from $to | out-null }
    } else {
      if ($symbolic) { cmd /c mklink $from $to | out-null }
      else           { cmd /c mklink /H $from $to | out-null }
    }

    show-success "Link to $to has been created at $from"
  }
}

function get-file {
<#
  .SYNOPSIS
  Download one or more files to a directory
  .PARAMETER urls
  One or more urls to download from
  .PARAMETER outputPaths
  One or more paths to save files to. See notes for how these
  work in combination with the urls.
  .EXAMPLE
  Download "robots.txt" to the "$HOME\Downloads" directory as "robots.txt"
  > get-file 'http://example.com/robots.txt' -to "$HOME\Downloads"
  .EXAMPLE
  Download "robots.txt" to the current directory as "robots.txt"
  > get-file 'http://example.com/robots.txt'
  .EXAMPLE
  Download "robots.txt" to a specific path
  > get-file 'http://example.com/robots.txt' -to "$HOME\Downloads\example-com_robots.txt"
  .EXAMPLE
  Download multiple files to the "$HOME\Downloads" directory
  > get-file 'http://example.com/robots.txt' 'http://example.com/index.html' -to "$HOME\Downloads"
  .EXAMPLE
  Download multiple files to the current directory with the given names
  > get-file 'http://example.com/robots.txt' 'http://example.com/index.html' -to "robots", "index"
  .NOTES
  Will autodetect proxies, and unless a specific path is provided, the last segment of the download URI is
  used as the filename, UNLESS the headers of the response provide a filename, in which case that will be used.
  If the specified download path does not exist, it will be created for you.

  If downloading multiple files, if you provide no output path, the files will be downloaded to the current
  directory. If you provide a single path that is a directory (determined either by the type of entity at
  that path, or by the lack of extension on the path), the files will be downloaded to that location
  instead. If you provide a one or more paths that are not directories (again determined by presence of
  an extension), then the paths will be associated with the files being downloaded in order of appearance.
  For any files which do not have an output path specified, the current directory will be used as a fallback.
  #>

  param(
    [parameter(ValueFromPipeline=$true)]
    [string[]]
    [ValidateNotNullOrEmpty()]
    $urls = ${ throw "You must provide at least one file to download!" },

    [string[]]
    [ValidateNotNullOrEmpty()]
    $outputPaths = @((pwd).Path)
  )

  $client   = new-object System.Net.WebClient
  $proxy    = [System.Net.WebRequest]::DefaultWebProxy -as [System.Net.WebProxy]
  $proxyUri = $proxy.Address.AbsoluteUri
  if (!(test-isEmpty $proxyUri)) {
    $client.Proxy = $proxy
  }

  # Gather metadata about outputs
  $outputs   = @{}
  foreach ($path in $outputPaths) {
    $info        = new-object System.IO.DirectoryInfo($path)
    $isDirectory = $info.Attributes.HasFlag([System.IO.FileAttributes]::Directory)
    # If it doesn't exist and it's a directory, create it
    if (!$info.Exists -and $info.Extension -eq "") {
      $isDirectory = $true
      new-directory $info.FullName
    }
    $outputs.Add($info.Fullname, @{"IsDirectory" = $isDirectory; "Name" = $info.Name})
  }

  # Combine information about inputs and outputs into download info
  $downloads = @{}
  foreach ($url in $urls) {
    $outputPath = $outputs.Keys
  }

  # TODO
    $tp = test-path $DestinationPath
    if ($tp -eq $false) { New-Item $DestinationPath -ItemType directory -Force | Out-Null }

    $Url | % {
        $regex = [Regex]::Split($_, "/")
        $file = Join-Path $DestinationPath $regex[$regex.Length - 1]

        Write-Host $file -NoNewline
        $wClient.DownloadFile($_, $file)
        Write-Host " 100%"
    }
}