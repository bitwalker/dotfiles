<#
  .SYNOPSIS
  Contains functions related to cryptography
  .NOTES
  - Currently only contains convenience functions for hashing.
  - DO NOT rely on these functions for serious crypto work, they are just here
    to make common testing or validation tasks easier
#>

# Ensure errors stop execution so that they can be caught
$ErrorActionPreference = 'Stop'
# Use stricter evaluation for this module
set-strictmode -version latest

function Get-Hash {
<#
  .SYNOPSIS
  Product a hash of the provided string or file
  .PARAMETER str
  Required. Can be either a string, or a path to a file.
  .PARAMETER algorithm
  Required. Valid values are: MD5, RIPEMD160, SHA1, SHA256, SHA384, SHA512.
  .INPUTS
  String (optional). Allows you to pipe the $str argument.
  .OUTPUTS
  String
  .EXAMPLE
  > get-hash "password" md5
  5f4dcc3b5aa765d61d8327deb882cf99
#>
  param(
    [parameter(ValueFromPipeline=$true)]
    [string] $str,
    [parameter(Mandatory=$true)]
    [validateset('md5', 'ripemd160', 'sha1', 'sha256', 'sha384', 'sha512')]
    [string] $algorithm
  )

  switch (![string]::IsNullOrWhiteSpace($str) -and (test-path $str)) {
    $true { get-filehash $str -algorithm $algorithm | % { $_.Hash.ToLowerInvariant() } }
    $false {
      $builder  = new-object System.Text.StringBuilder
      $hasher   = [System.Security.Cryptography.HashAlgorithm]::Create($algorithm)
      $encoding = [System.Text.Encoding]::UTF8
      $bytes    = $hasher.ComputeHash($encoding.GetBytes($str))
      $bytes | foreach { [void]$builder.Append($_.ToString("x2")) }
      $builder.ToString()
    }
  }
}

function Get-MD5([string]$str) {
<#
  .SYNOPSIS
  Produce an MD5 hash of the provided string or file
  .EXAMPLE
  > get-md5 "password"
  5f4dcc3b5aa765d61d8327deb882cf99
  .LINK
  get-hash
#>
  get-hash $str -algorithm md5
}

function Get-SHA1([string]$str) {
<#
  .SYNOPSIS
  Produce a SHA1 hash of the provided string or file
  .EXAMPLE
  > get-sha1 "password"
  5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8
  .LINK
  get-hash
#>
  get-hash $str -algorithm sha1
}

function Get-SHA256([string]$str) {
<#
  .SYNOPSIS
  Produce a SHA256 hash of the provided string or file
  .EXAMPLE
  > get-sha256 "password"
  5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8
  .LINK
  get-hash
#>
  get-hash $str -algorithm sha256
}