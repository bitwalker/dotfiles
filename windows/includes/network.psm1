<#
  .SYNOPSIS
  General helper functions that don't fit much of anywhere else
#>
$ErrorActionPreference = 'Stop'

set-strictmode -version latest

$_includes = $PSScriptRoot
import-module (join-path $_includes logging.psm1)
import-module (join-path $_includes functional.psm1)
import-module (join-path $_includes helpers.psm1)

function show-network([switch]$detailed = $false) {
  # Fetch info
  $summary = get-netipconfiguration -detailed | % {
    $profile = $_.NetProfile
    if ($profile -eq $null) {
      $profile = new-object psobject -property @{
        Name = "local network";
        NetworkCategory = "Private";
        IPv4Connectivity = "Disconnected";
        IPv6Connectivity = "Disconnected";
      }
    }
    new-object psobject -property @{
      InterfaceIndex  = $_.InterfaceIndex;
      Name            = $_.InterfaceAlias;
      Description     = $_.InterfaceDescription;
      Type            = $_.NetAdapter.PhysicalMediaType;
      Status          = $_.NetAdapter.Status;
      State           = $_.NetAdapter.MediaConnectionState;
      IsConnected     = $_.NetAdapter.ConnectorPresent;
      UploadSpeed     = "$($_.NetAdapter.TransmitLinkSpeed / 1000000) mbps";
      DownloadSpeed   = "$($_.NetAdapter.ReceiveLinkSpeed / 1000000) mbps";
      NetworkName     = $profile.Name;
      NetworkCategory = $profile.NetworkCategory;
      Hostname        = $_.NetAdapter.SystemName;
      IPv4            = $_.IPV4Address;
      IPv4Scope       = $profile.IPv4Connectivity;
      IPv6            = $_.IPV6Address;
      IPv6Scope       = $profile.IPv6Connectivity;
      MacAddress      = $_.NetAdapter.MacAddress;
      DNSServer       = $_.DNSServer | % { $_ }
      Driver = new-object psobject -property @{
        Name        = $_.NetAdapter.DriverFileName;
        Description = $_.NetAdapter.DriverInformation;
        Path        = $_.NetAdapter.DriverName;
        Provider    = $_.NetAdapter.DriverProvider;
      };
      Flags = new-object psobject -property @{
        InterfacePaused   = $_.NetAdapter.OperationalStatusDownInterfacePaused;
        LowPowerState     = $_.NetAdapter.OperationalStatusDownLowPowerState;
        MediaDisconnected = $_.NetAdapter.OperationalStatusDownMediaDisconnected;
        PromiscuousMode   = $_.NetAdapter.PromiscuousMode;
      };
    }
  }
  # Display
  if ($detailed) {
    $adapters = @{}
    $summary | foreach { $adapters.Add($_.name, $_) }
    return new-object psobject $adapters
  }
  else {
    $summary | foreach {
      $adapter = $_
      switch ($adapter.state) {
        "connected" { # Enabled, connected
          $ip4_internet = ($adapter.ipv4scope -match 'internet')
          $ip6_internet = ($adapter.ipv6scope -match 'internet')
          show-info "$($adapter.name) is active and currently connected to $($adapter.networkname) as $($adapter.hostname)"
          show-info "`tIs Private: $($adapter.networkcategory -match 'private')"
          switch ("$ip4_internet|$ip6_internet") {
            "$true|$true" {
              show-info "`tConnected to internet over IPv4 and IPv6"
              show-info "`tAddresses: $($adapter.ipv4[0].ipaddress), $($adapter.ipv6[0].ipaddress)"
            }
            "$false|$true" {
              show-alert "`tConnected to internet over IPv6 only"
              show-alert "`tAddress: $($adapter.ipv6[0].ipaddress)"
            }
            "$true|$false" {
              show-alert "`tConnected to internet over IPv4 only"
              show-alert "`tAddress: $($adapter.ipv4[0].ipaddress)"
            }
            "$false|$false" {
              $valid_ips = ($adapter.ipv4 + $adapter.ipv6) | where { $_ -ne $null }
              $valid_str = [string]::join(",", $valid_ips)
              show-warning "`tNot connected to the internet!"
              show-warning "`tAssigned Addresses: $valid_str"
            }
          }
        }
        "disconnected" { # Enabled, not connected
          show-warning "$($adapter.name) is not connected to any networks."
        }
        "unknown"   { # Disabled, many possible reasons
          switch ($adapter.status) {
            "not present" { show-error "$($adapter.name) is currently disabled."; break }
            default { show-error "$($adapter.name) is in an unknown state: $($adapter.status)"; break }
          }
        }
      }
    }
  }
}

function test-isNetworkAddress([string]$address) {
<#
  .SYNOPSIS
  Tests whether the provided network address is valid or not.
  .PARAMETER address
  The string to test
#>
  trap { return $false; }

  # Verify the address is valid format
  switch -regex ($address) {
    '\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}' {
      <# IPv4 Address #> $true; break
    }
    '[\w\d]{4}::[\w\d]{4}:[\w\d]{4}:[\w\d]{4}:[\w\d]{4}(%\d)?' {
      <# IPv6 Address #> $true; break
    }
    default { return $false; }
  }
  # Test that the address is available
  test-connection -computername $address `
                  -count 1 | `
                  -quiet `
}

function add-host([string]$hostname, [string]$ip = '127.0.0.1', [string]$description = $hostname) {
<#
  .SYNOPSIS
  Add entry to hosts file. Requires an elevated shell to run.
  .PARAMETER hostname
  Required. The host name of the entry.
  .PARAMETER ip
  Optional. The host IP address of the entry. Defaults to 127.0.0.1.
  .PARAMETER description
  Optional. A short description of the entity. Defaults to $hostname.
  .EXAMPLE
  > add-host example.com 210.105.0.1 'An example host entry'
  Host entry created for example.com, host bound to 210.105.0.1
  .EXAMPLE
  > add-host example.com
#>
  # Ensure we're an admin
  assert-isElevatedShell

  # Validate IP address


  # Fetch current host file contents
  $path       = "$env:windir\system32\drivers\etc\hosts"
  $contents   = get-content $path
  $last_entry = $contents[$contents.Count - 1]

  # Construct new host entry
  $host_entry = "`t$ip`t$hostname`t# $description`n"

  # Determine where to insert new content
  $contents = if (test-isEmpty($last_entry)) {
    # Blank line, just add the host entry
    ($contents + $host_entry)
  } else {
    # Has text, check to see if it's a host entry, if so, add the new
    # entry to the next line. If not, add two newlines, then add our entry
    switch ($last_entry -match '(#)?\s*(([:]{2}\d)|(\d+\.\d+\.\d+\.\d+))\s+(.*)') {
      $true  { ($contents + $host_entry) }
      $false { ($contents + "`n" + $host_entry) }
    }
  }

  # Save file
  $contents | out-file -filepath $path -encoding ascii -force

  # Notify user
  show-succes "Host entry created for $hostname, host bound to $ip"
}

function remove-host([string]$hostname) {
<#
  .SYNOPSIS
  Remove entry from hosts file, by hostname
#>
  assert-isElevatedShell
  # Fetch current host file contents
  $path     = "$env:windir\system32\drivers\etc\hosts"
  $contents = get-content $path
}
