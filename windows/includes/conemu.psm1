<#
  .SYNOPSIS
  Ccontains helpers for importing/exporting ConEmu preferences
#>

set-strictmode -version latest

function Export-ConEmuPrefs([string]$outputPath = ${ throw "Must provide an output path!" }) {
<#
  .SYNOPSIS
  Exports ConEmu preferences to the provided path.
  .DESCRIPTION
  Exports ConEmu preferences to the given path. If the path is a directory reference,
  the filename will be ConEmu.reg, otherwise the filename defined in the path will be used.
  If the path does not yet exist, it will be created.
  .PARAMETER outputPath
  Required. An absolute or relative path to which the preferences file will be saved.
  .EXAMPLE
  > export-conemu $HOME\myconemu.reg
#>
  $fi       = new System.IO.FileInfo((resolve-path $outputPath))
  $export   = tuple @{"dir" = (split-path fi.FullName); "filename" = $fi.Name}
  if ($fi.Extension -eq $null -and !$fi.Exists) {
    mkdirp $fi.FullName
    $export.dir      = $fi.FullName
    $export.filename = "ConEmu.reg"
  }
  $outputPath = (join-path $export.dir $export.filename)
  reg export HKCU\Software\ConEmu $outputPath | out-null
  write-host "Your ConEmu preferences have been exported to $outputPath"
}

function Import-ConEmuPrefs([string]$inputPath = ${ throw "Must provide a valid file path!" }) {
<#
  .SYNOPSIS
  Imports ConEmu preferences from the provided path.
  .DESCRIPTION
  Imports ConEmu preferences given a path to a registry data file (*.reg) containing the
  desired ConEmu preferences.
  .PARAMETER inputPath
  Required. An absolute or relative path to the ConEmu preferences file (.reg)
  .EXAMPLE
  > import-conemu .\ConEmu.reg
  Your ConEmu preferences have been imported!
#>
  $fi = new System.IO.FileInfo((resolve-path $inputPath))

  if (!$fi.Exists)              { throw "$inputPath does not exist!" }
  if ($fi.Extension -ne ".reg") { throw "$inputPath is not a valid registry file!" }

  reg import $fi.FullName | out-null
  write-host "Your ConEmu preferences have been imported!" -foregroundcolor green
}