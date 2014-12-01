& {
Set-StrictMode -Off
# removes any empty (blank) line that is positioned
# - between opening brace and content
# - between content and closing brace

$crlf = -join [Char[]](13,10)


$items = [ISESteroids.SteroidsFixer.Helpers]::GetAST('ScriptAndStatementblock')
foreach($ScriptBlock in $items)
{
  $startoffset = $ScriptBlock.Extent.StartOffset+1
  
  
  # are start and end on the same line (one-liner)?
  if ($ScriptBlock.Extent.StartLineNumber -eq $ScriptBlock.Extent.EndLineNumber) { continue }
  
  # get scriptblock content
  $text = $ScriptBlock.Extent.Text.Trim()
  
  # is the scriptblock enclosed by braces?
  # the top scriptblock in a script is not enclosed, to exit in this case:
  if ($text.StartsWith('{') -eq $false) { continue }
  
  # at this point, we have a scriptblock that may need reformatting
  
  # is there any non-whitespace content following before getting to a line break?
  # if so, add a line break right after the opening brace
  # do not do this if this script block is a parameter argument, though
  $parentStructure = if ($ScriptBlock.Parent -ne $null)
  {
    $parentStructure = $ScriptBlock.Parent.GetType().Name
  }
  else
  {
    $parentStructure = ''
  }
  
  # how many real lines are in this block?
  $nonwhitespace = $ScriptBlock.Extent.Text.SubString(1, $ScriptBlock.Extent.Text.Length-2) -replace '[^\S\n]' -split '\n' | Where-Object { $_.Trim() }
  $lines = $nonwhitespace.Count
  $linebreak = ''
  if ($lines -gt 1 )
  { $linebreak = "`n" }
  
  # is there empty space between opening brace and content?
  $token = [ISESteroids.SteroidsFixer.Helpers]::GetNextNonWhitespaceToken($ScriptBlock.Extent.StartOffset, $ScriptBlock.Extent.EndOffset, 'Forward')
  if ($token -ne $null)
  {
    $start = $ScriptBlock.Extent.StartOffset + 1
    $end = $token.Extent.StartOffset
    if ($start -ne $end)
    {
      [ISESteroids.SteroidsFixer.Helpers]::AddTextChange($start, $end, $linebreak)
    }
  }
  
  # is there empty space between content and closing brace?
  $token = [ISESteroids.SteroidsFixer.Helpers]::GetNextNonWhitespaceToken($ScriptBlock.Extent.EndOffset-1, $ScriptBlock.Extent.StartOffset+1, 'Backwards')
  if ($token -ne $null)
  {
    # is this a regular comment token? In this case, brace cannot be moved
    $isComment = (($token.Kind -eq 'Comment') -and ($token.Extent.Text.StartsWith('<#') -eq $false))
    
      $start = $token.Extent.EndOffset
      $end = $ScriptBlock.Extent.EndOffset - 1
      
      # if the token is a classic comment, add one linebreak or else the brace will be in the comment line (thus commented out)
      $addText = ''
      if ($isComment -or $Lines -gt 1) { 
      $addText = $crlf 
      }
      if ($start -ne $end)
      {
        [ISESteroids.SteroidsFixer.Helpers]::AddTextChange($start, $end, $addText)
      }
    
  }
}
Invoke-SteroidsTextChange
}
# SIG # Begin signature block
# MIId1AYJKoZIhvcNAQcCoIIdxTCCHcECAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUK0vE5AZtBlx2jRi3t0Dd6aWp
# adqgghkGMIIFHDCCBASgAwIBAgIQAp+oNhrWxpP858ywp8f2ezANBgkqhkiG9w0B
# AQUFADBvMQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYD
# VQQLExB3d3cuZGlnaWNlcnQuY29tMS4wLAYDVQQDEyVEaWdpQ2VydCBBc3N1cmVk
# IElEIENvZGUgU2lnbmluZyBDQS0xMB4XDTE0MDkwNDAwMDAwMFoXDTE1MDkwOTEy
# MDAwMFowcjELMAkGA1UEBhMCREUxFjAUBgNVBAgTDU5pZWRlcnNhY2hzZW4xETAP
# BgNVBAcTCEhhbm5vdmVyMRswGQYDVQQKExJUb2JpYXMgRHIuIFdlbHRuZXIxGzAZ
# BgNVBAMTElRvYmlhcyBEci4gV2VsdG5lcjCCASIwDQYJKoZIhvcNAQEBBQADggEP
# ADCCAQoCggEBAJUaFBmSOaJ2atB6X772tQWjbFYSUPeKkcQ6piLwrA2TS1I/175p
# R7UzgpPpuJ2/Gkqag/uZOS0SUwQm+Z6Y2TfkyzcyOJdO0kuwsl+/nJqVN97xIABt
# P3a3oVwxjo7BDmLFxMIPxint8bu5zy9LL2e3AUtH1ikTOrzo0qbJSJLorMlRZcgp
# dDg1gSloEUHeZBOBX3hgMQQFnk5lK6UeopqaxBd0S1BCYUEI2hJxJKerns0MmX9O
# ZppYz8giZo5Q7/bnRB73jT5YZjVQr2bnCuci1sz12KshwXvc6If+QqqKy6LsTWgh
# MD3n8J61+/+bbLuk8JzDlLciU07o1cNbdJ0CAwEAAaOCAa8wggGrMB8GA1UdIwQY
# MBaAFHtozimqwBe+SXrh5T/Wp/dFjzUyMB0GA1UdDgQWBBR8XU372FlNMblKRzWM
# 2+VbuGAYJDAOBgNVHQ8BAf8EBAMCB4AwEwYDVR0lBAwwCgYIKwYBBQUHAwMwbQYD
# VR0fBGYwZDAwoC6gLIYqaHR0cDovL2NybDMuZGlnaWNlcnQuY29tL2Fzc3VyZWQt
# Y3MtZzEuY3JsMDCgLqAshipodHRwOi8vY3JsNC5kaWdpY2VydC5jb20vYXNzdXJl
# ZC1jcy1nMS5jcmwwQgYDVR0gBDswOTA3BglghkgBhv1sAwEwKjAoBggrBgEFBQcC
# ARYcaHR0cHM6Ly93d3cuZGlnaWNlcnQuY29tL0NQUzCBggYIKwYBBQUHAQEEdjB0
# MCQGCCsGAQUFBzABhhhodHRwOi8vb2NzcC5kaWdpY2VydC5jb20wTAYIKwYBBQUH
# MAKGQGh0dHA6Ly9jYWNlcnRzLmRpZ2ljZXJ0LmNvbS9EaWdpQ2VydEFzc3VyZWRJ
# RENvZGVTaWduaW5nQ0EtMS5jcnQwDAYDVR0TAQH/BAIwADANBgkqhkiG9w0BAQUF
# AAOCAQEAgl6LemxL/XV0RqQqf32Yr1a0CM9vEjT0RUhBW+dCOlb6qIz7McSogSzk
# trrSmjNMjX5FzP/uTtDC38VGPV+iX1fENUWiYUBg7ZgxxGX25+uLxNltnTV69Xv8
# d+tfx6zBj8DOhet79v9GYJbBDmIoDdPOwC9fLFwrk0HeRs0Uc1bDCdU/6GJ9M7ho
# 9bDmcnsYaHrkwWyNzZvc2e0fpJD1e/KSRYHYT6Q15XAdn9neJewlBPoIAxgJsfHB
# Fo2pIYYTm7nRcY0zZEJUw8YMvt6XmnhTm88720vTy0pJM4T1KCMmgDdfgVC2jxWa
# RA3a/HWK+Srlku9b/W9qYuXLPVcPcDCCBmowggVSoAMCAQICEAZkAUbpgOAOYKFN
# j0RKWVgwDQYJKoZIhvcNAQEFBQAwYjELMAkGA1UEBhMCVVMxFTATBgNVBAoTDERp
# Z2lDZXJ0IEluYzEZMBcGA1UECxMQd3d3LmRpZ2ljZXJ0LmNvbTEhMB8GA1UEAxMY
# RGlnaUNlcnQgQXNzdXJlZCBJRCBDQS0xMB4XDTE0MDUyMDAwMDAwMFoXDTE1MDYw
# MzAwMDAwMFowRzELMAkGA1UEBhMCVVMxETAPBgNVBAoTCERpZ2lDZXJ0MSUwIwYD
# VQQDExxEaWdpQ2VydCBUaW1lc3RhbXAgUmVzcG9uZGVyMIIBIjANBgkqhkiG9w0B
# AQEFAAOCAQ8AMIIBCgKCAQEAqYkY9jz0cTh/7Ea2LfjJAyUlCT023BpNjNgLP54H
# JlwVkORvGporGxGeg3bTVAcr5INCpjtt7PPznWyPVpqBqdnArxE4opqA4pAU/BGX
# QqZQnS6ps+BIVy8ESRr1110jm6B3EOxzxW8a7c3WExRbVdDwQTBGSMAuegKq4A6O
# Ef3jJMowO/mYp7vgJ6lpE8jazn41/OFF93zyZBRIQZgDH86IymgeEI/xlKHYbCvw
# vWuRhZXZO4VMlpAv8S3nWAMjgNTM0ehaplIaEa5jR1qqsz8iYFH2/tK5jQQtP7Wr
# NXXqZNNM+tBAdZIEJqXCLyzh2+vBa++Y9NAkNY8ewBQWQQIDAQABo4IDNTCCAzEw
# DgYDVR0PAQH/BAQDAgeAMAwGA1UdEwEB/wQCMAAwFgYDVR0lAQH/BAwwCgYIKwYB
# BQUHAwgwggG/BgNVHSAEggG2MIIBsjCCAaEGCWCGSAGG/WwHATCCAZIwKAYIKwYB
# BQUHAgEWHGh0dHBzOi8vd3d3LmRpZ2ljZXJ0LmNvbS9DUFMwggFkBggrBgEFBQcC
# AjCCAVYeggFSAEEAbgB5ACAAdQBzAGUAIABvAGYAIAB0AGgAaQBzACAAQwBlAHIA
# dABpAGYAaQBjAGEAdABlACAAYwBvAG4AcwB0AGkAdAB1AHQAZQBzACAAYQBjAGMA
# ZQBwAHQAYQBuAGMAZQAgAG8AZgAgAHQAaABlACAARABpAGcAaQBDAGUAcgB0ACAA
# QwBQAC8AQwBQAFMAIABhAG4AZAAgAHQAaABlACAAUgBlAGwAeQBpAG4AZwAgAFAA
# YQByAHQAeQAgAEEAZwByAGUAZQBtAGUAbgB0ACAAdwBoAGkAYwBoACAAbABpAG0A
# aQB0ACAAbABpAGEAYgBpAGwAaQB0AHkAIABhAG4AZAAgAGEAcgBlACAAaQBuAGMA
# bwByAHAAbwByAGEAdABlAGQAIABoAGUAcgBlAGkAbgAgAGIAeQAgAHIAZQBmAGUA
# cgBlAG4AYwBlAC4wCwYJYIZIAYb9bAMVMB8GA1UdIwQYMBaAFBUAEisTmLKZB+0e
# 36K+Vw0rZwLNMB0GA1UdDgQWBBQ0/A9Gfqu32Wtd+FNyReYEkdPC+zB9BgNVHR8E
# djB0MDigNqA0hjJodHRwOi8vY3JsMy5kaWdpY2VydC5jb20vRGlnaUNlcnRBc3N1
# cmVkSURDQS0xLmNybDA4oDagNIYyaHR0cDovL2NybDQuZGlnaWNlcnQuY29tL0Rp
# Z2lDZXJ0QXNzdXJlZElEQ0EtMS5jcmwwdwYIKwYBBQUHAQEEazBpMCQGCCsGAQUF
# BzABhhhodHRwOi8vb2NzcC5kaWdpY2VydC5jb20wQQYIKwYBBQUHMAKGNWh0dHA6
# Ly9jYWNlcnRzLmRpZ2ljZXJ0LmNvbS9EaWdpQ2VydEFzc3VyZWRJRENBLTEuY3J0
# MA0GCSqGSIb3DQEBBQUAA4IBAQAQQJCzcZ/wHjsC3D+8TBXnDjkSwIZAFhqgZciT
# W8MHsKSIRA+QlEdeNcMFsv7YnR7U9Ld+xgchXmYP5TWypB8EKkIw5idPJ2I0wGaU
# wgIvR3kmSp1KXAS1BEjzK3lJGkcUvBblx7hnUCC9rMS0/ashgCxgphurfO8HPEDu
# aRhN1ifiNFnhKUIjsz1DnW4el8TdgvjyoRT4jfxmfWTsQDcBBN5MVU4/0yL4Rs8u
# WMDLsKW+4OUbi0hcshGRAsy5XOz9HnUl/n4lFrosEoQf2/EO+QRTsvhNnAZGM3F1
# hobjVA/X67PVWK8rWofEoDANM8am8TC+1ft/Nb7G4o/ZGe+UMIIGozCCBYugAwIB
# AgIQD6hJBhXXAKC+IXb9xextvTANBgkqhkiG9w0BAQUFADBlMQswCQYDVQQGEwJV
# UzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3d3cuZGlnaWNlcnQu
# Y29tMSQwIgYDVQQDExtEaWdpQ2VydCBBc3N1cmVkIElEIFJvb3QgQ0EwHhcNMTEw
# MjExMTIwMDAwWhcNMjYwMjEwMTIwMDAwWjBvMQswCQYDVQQGEwJVUzEVMBMGA1UE
# ChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3d3cuZGlnaWNlcnQuY29tMS4wLAYD
# VQQDEyVEaWdpQ2VydCBBc3N1cmVkIElEIENvZGUgU2lnbmluZyBDQS0xMIIBIjAN
# BgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnHz5oI8KyolLU5o87BkifwzL90hE
# 0D8ibppP+s7fxtMkkf+oUpPncvjxRoaUxasX9Hh/y3q+kCYcfFMv5YPnu2oFKMyg
# FxFLGCDzt73y3Mu4hkBFH0/5OZjTO+tvaaRcAS6xZummuNwG3q6NYv5EJ4KpA8P+
# 5iYLk0lx5ThtTv6AXGd3tdVvZmSUa7uISWjY0fR+IcHmxR7J4Ja4CZX5S56uzDG9
# alpCp8QFR31gK9mhXb37VpPvG/xy+d8+Mv3dKiwyRtpeY7zQuMtMEDX8UF+sQ0R8
# /oREULSMKj10DPR6i3JL4Fa1E7Zj6T9OSSPnBhbwJasB+ChB5sfUZDtdqwIDAQAB
# o4IDQzCCAz8wDgYDVR0PAQH/BAQDAgGGMBMGA1UdJQQMMAoGCCsGAQUFBwMDMIIB
# wwYDVR0gBIIBujCCAbYwggGyBghghkgBhv1sAzCCAaQwOgYIKwYBBQUHAgEWLmh0
# dHA6Ly93d3cuZGlnaWNlcnQuY29tL3NzbC1jcHMtcmVwb3NpdG9yeS5odG0wggFk
# BggrBgEFBQcCAjCCAVYeggFSAEEAbgB5ACAAdQBzAGUAIABvAGYAIAB0AGgAaQBz
# ACAAQwBlAHIAdABpAGYAaQBjAGEAdABlACAAYwBvAG4AcwB0AGkAdAB1AHQAZQBz
# ACAAYQBjAGMAZQBwAHQAYQBuAGMAZQAgAG8AZgAgAHQAaABlACAARABpAGcAaQBD
# AGUAcgB0ACAAQwBQAC8AQwBQAFMAIABhAG4AZAAgAHQAaABlACAAUgBlAGwAeQBp
# AG4AZwAgAFAAYQByAHQAeQAgAEEAZwByAGUAZQBtAGUAbgB0ACAAdwBoAGkAYwBo
# ACAAbABpAG0AaQB0ACAAbABpAGEAYgBpAGwAaQB0AHkAIABhAG4AZAAgAGEAcgBl
# ACAAaQBuAGMAbwByAHAAbwByAGEAdABlAGQAIABoAGUAcgBlAGkAbgAgAGIAeQAg
# AHIAZQBmAGUAcgBlAG4AYwBlAC4wEgYDVR0TAQH/BAgwBgEB/wIBADB5BggrBgEF
# BQcBAQRtMGswJAYIKwYBBQUHMAGGGGh0dHA6Ly9vY3NwLmRpZ2ljZXJ0LmNvbTBD
# BggrBgEFBQcwAoY3aHR0cDovL2NhY2VydHMuZGlnaWNlcnQuY29tL0RpZ2lDZXJ0
# QXNzdXJlZElEUm9vdENBLmNydDCBgQYDVR0fBHoweDA6oDigNoY0aHR0cDovL2Ny
# bDMuZGlnaWNlcnQuY29tL0RpZ2lDZXJ0QXNzdXJlZElEUm9vdENBLmNybDA6oDig
# NoY0aHR0cDovL2NybDQuZGlnaWNlcnQuY29tL0RpZ2lDZXJ0QXNzdXJlZElEUm9v
# dENBLmNybDAdBgNVHQ4EFgQUe2jOKarAF75JeuHlP9an90WPNTIwHwYDVR0jBBgw
# FoAUReuir/SSy4IxLVGLp6chnfNtyA8wDQYJKoZIhvcNAQEFBQADggEBAHtyHWT/
# iMg6wbfp56nEh7vblJLXkFkz+iuH3qhbgCU/E4+bgxt8Q8TmjN85PsMV7LDaOyEl
# eyTBcl24R5GBE0b6nD9qUTjetCXL8KvfxSgBVHkQRiTROA8moWGQTbq9KOY/8cSq
# m/baNVNPyfI902zcI+2qoE1nCfM6gD08+zZMkOd2pN3yOr9WNS+iTGXo4NTa0cfI
# kWotI083OxmUGNTVnBA81bEcGf+PyGubnviunJmWeNHNnFEVW0ImclqNCkojkkDo
# ht4iwpM61Jtopt8pfwa5PA69n8SGnIJHQnEyhgmZcgl5S51xafVB/385d2TxhI2+
# ix6yfWijpZCxDP8wggbNMIIFtaADAgECAhAG/fkDlgOt6gAK6z8nu7obMA0GCSqG
# SIb3DQEBBQUAMGUxCzAJBgNVBAYTAlVTMRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMx
# GTAXBgNVBAsTEHd3dy5kaWdpY2VydC5jb20xJDAiBgNVBAMTG0RpZ2lDZXJ0IEFz
# c3VyZWQgSUQgUm9vdCBDQTAeFw0wNjExMTAwMDAwMDBaFw0yMTExMTAwMDAwMDBa
# MGIxCzAJBgNVBAYTAlVTMRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsT
# EHd3dy5kaWdpY2VydC5jb20xITAfBgNVBAMTGERpZ2lDZXJ0IEFzc3VyZWQgSUQg
# Q0EtMTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAOiCLZn5ysJClaWA
# c0Bw0p5WVFypxNJBBo/JM/xNRZFcgZ/tLJz4FlnfnrUkFcKYubR3SdyJxArar8te
# a+2tsHEx6886QAxGTZPsi3o2CAOrDDT+GEmC/sfHMUiAfB6iD5IOUMnGh+s2P9gw
# w/+m9/uizW9zI/6sVgWQ8DIhFonGcIj5BZd9o8dD3QLoOz3tsUGj7T++25VIxO4e
# s/K8DCuZ0MZdEkKB4YNugnM/JksUkK5ZZgrEjb7SzgaurYRvSISbT0C58Uzyr5j7
# 9s5AXVz2qPEvr+yJIvJrGGWxwXOt1/HYzx4KdFxCuGh+t9V3CidWfA9ipD8yFGCV
# /QcEogkCAwEAAaOCA3owggN2MA4GA1UdDwEB/wQEAwIBhjA7BgNVHSUENDAyBggr
# BgEFBQcDAQYIKwYBBQUHAwIGCCsGAQUFBwMDBggrBgEFBQcDBAYIKwYBBQUHAwgw
# ggHSBgNVHSAEggHJMIIBxTCCAbQGCmCGSAGG/WwAAQQwggGkMDoGCCsGAQUFBwIB
# Fi5odHRwOi8vd3d3LmRpZ2ljZXJ0LmNvbS9zc2wtY3BzLXJlcG9zaXRvcnkuaHRt
# MIIBZAYIKwYBBQUHAgIwggFWHoIBUgBBAG4AeQAgAHUAcwBlACAAbwBmACAAdABo
# AGkAcwAgAEMAZQByAHQAaQBmAGkAYwBhAHQAZQAgAGMAbwBuAHMAdABpAHQAdQB0
# AGUAcwAgAGEAYwBjAGUAcAB0AGEAbgBjAGUAIABvAGYAIAB0AGgAZQAgAEQAaQBn
# AGkAQwBlAHIAdAAgAEMAUAAvAEMAUABTACAAYQBuAGQAIAB0AGgAZQAgAFIAZQBs
# AHkAaQBuAGcAIABQAGEAcgB0AHkAIABBAGcAcgBlAGUAbQBlAG4AdAAgAHcAaABp
# AGMAaAAgAGwAaQBtAGkAdAAgAGwAaQBhAGIAaQBsAGkAdAB5ACAAYQBuAGQAIABh
# AHIAZQAgAGkAbgBjAG8AcgBwAG8AcgBhAHQAZQBkACAAaABlAHIAZQBpAG4AIABi
# AHkAIAByAGUAZgBlAHIAZQBuAGMAZQAuMAsGCWCGSAGG/WwDFTASBgNVHRMBAf8E
# CDAGAQH/AgEAMHkGCCsGAQUFBwEBBG0wazAkBggrBgEFBQcwAYYYaHR0cDovL29j
# c3AuZGlnaWNlcnQuY29tMEMGCCsGAQUFBzAChjdodHRwOi8vY2FjZXJ0cy5kaWdp
# Y2VydC5jb20vRGlnaUNlcnRBc3N1cmVkSURSb290Q0EuY3J0MIGBBgNVHR8EejB4
# MDqgOKA2hjRodHRwOi8vY3JsMy5kaWdpY2VydC5jb20vRGlnaUNlcnRBc3N1cmVk
# SURSb290Q0EuY3JsMDqgOKA2hjRodHRwOi8vY3JsNC5kaWdpY2VydC5jb20vRGln
# aUNlcnRBc3N1cmVkSURSb290Q0EuY3JsMB0GA1UdDgQWBBQVABIrE5iymQftHt+i
# vlcNK2cCzTAfBgNVHSMEGDAWgBRF66Kv9JLLgjEtUYunpyGd823IDzANBgkqhkiG
# 9w0BAQUFAAOCAQEARlA+ybcoJKc4HbZbKa9Sz1LpMUerVlx71Q0LQbPv7HUfdDjy
# slxhopyVw1Dkgrkj0bo6hnKtOHisdV0XFzRyR4WUVtHruzaEd8wkpfMEGVWp5+Pn
# q2LN+4stkMLA0rWUvV5PsQXSDj0aqRRbpoYxYqioM+SbOafE9c4deHaUJXPkKqvP
# nHZL7V/CSxbkS3BMAIke/MV5vEwSV/5f4R68Al2o/vsHOE8Nxl2RuQ9nRc3Wg+3n
# kg2NsWmMT/tZ4CMP0qquAHzunEIOz5HXJ7cW7g/DvXwKoO4sCFWFIrjrGBpN/Coh
# rUkxg0eVd3HcsRtLSxwQnHcUwZ1PL1qVCCkQJjGCBDgwggQ0AgEBMIGDMG8xCzAJ
# BgNVBAYTAlVTMRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5k
# aWdpY2VydC5jb20xLjAsBgNVBAMTJURpZ2lDZXJ0IEFzc3VyZWQgSUQgQ29kZSBT
# aWduaW5nIENBLTECEAKfqDYa1saT/OfMsKfH9nswCQYFKw4DAhoFAKB4MBgGCisG
# AQQBgjcCAQwxCjAIoAKAAKECgAAwGQYJKoZIhvcNAQkDMQwGCisGAQQBgjcCAQQw
# HAYKKwYBBAGCNwIBCzEOMAwGCisGAQQBgjcCARUwIwYJKoZIhvcNAQkEMRYEFB1B
# kjC6nlnWEEiK1h27ioTUK4dYMA0GCSqGSIb3DQEBAQUABIIBAISFbfKGhcjj6u5w
# RfhxX4ORPA2BShinCvEWYks+DUnKhHyFCuuZ9My6OP8SBiAebwdq5fw0lkuVLiWi
# Xrkm4bonbojnd3tacPmGqb6GhPWaOnCsoWsee1y3Fodz3Dw4WNhRST1WQMDz0Md0
# vXPSaMdkuix+9JcL2w5Z/kP3XY/7sRO7oc8NHcCMBpZLv2Sc/AeKqsbXNh2Oiq3S
# hj7qWa5RT6OVQc+zndJUBatJpIFSl38sndt/Hb4dpg+3YA6SUyHYDd3/WWIwwi09
# 4P0lrCCBigaGJJ8nbB9sEJY4xyrvHfmvVW5JfGOWw/IqDgoKDS+ybqiEN1J34+uP
# V/1L9z2hggIPMIICCwYJKoZIhvcNAQkGMYIB/DCCAfgCAQEwdjBiMQswCQYDVQQG
# EwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3d3cuZGlnaWNl
# cnQuY29tMSEwHwYDVQQDExhEaWdpQ2VydCBBc3N1cmVkIElEIENBLTECEAZkAUbp
# gOAOYKFNj0RKWVgwCQYFKw4DAhoFAKBdMBgGCSqGSIb3DQEJAzELBgkqhkiG9w0B
# BwEwHAYJKoZIhvcNAQkFMQ8XDTE0MTAyNTExMzEyN1owIwYJKoZIhvcNAQkEMRYE
# FNNTfpCJRDfbclKYqT213nqFtY27MA0GCSqGSIb3DQEBAQUABIIBAGCEFLHdggAr
# 1yXBnBi2n1km8vzY8u5U4bk/NcSdHtP/gabO7znurFmRvagYnw1rulonfpCiSiwu
# bMZbu6NARD7YLVOS0cHqTtvTYWYkwoxtTF+O/gh48iedkUA+4HH5SxVx57rwKVp4
# s/V9wi4RZ3X8r4dVVPOM+PEiusqrnr2BJXDEBKX1+a2242chNrvYB1kT+mBECnDP
# 2L+Qxai/N0eD1ZcKFYak6W4rxLwh9ZrE1MkG9pcoRMhHW1U7v6bdOo9BxV0QuLmm
# pBa8uHVWQv15/qL3VIB1p/wYTTv5Ftgqkl6jYx1BHuzG9dIhyqG+J0YZfr3USrTn
# LGIyWQQZNXg=
# SIG # End signature block
