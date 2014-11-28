<#
  .synopsis
  This module provides common and useful date/time functions that aren't
  available out of the box with either .NET or Powershell.
#>

# Date/Time format strings
$local:DATETIME_FORMATS = @{
  'iso8601'     = 'yyyy-MM-ddTHH:mm:ssK';
  'iso8601_ext' = 'yyyy-MM-ddTHH:mm:ss.fffK';
}
# Mapping of common names to enumeration values
$local:DATETIME_KINDS = @{
  'local' = [DateTimeKind]::Local;
  'utc'   = [DateTimeKind]::Utc;
  'zulu'  = [DateTimeKind]::Utc;
}
# Formatting enumerations
$local:DATETIME_FORMAT_INFO = [System.Globalization.DateTimeFormatInfo]::InvariantInfo
$local:DATETIME_STYLE       = [System.Globalization.DateTimeStyles]::None
# Value of each time unit in terms of milliseconds
$local:DAYS_IN_YEAR = 365.242
$local:MILLISECOND  = 1
$local:SECOND       = $MILLISECOND * 1000
$local:MINUTE       = $SECOND * 60
$local:HOUR         = $MINUTE * 60
$local:DAY          = $HOUR * 24
$local:WEEK         = $DAY * 7
$local:MONTH        = $DAY * ($DAYS_IN_YEAR / 12)
$local:YEAR         = $DAYS_IN_YEAR * $DAY

function ConvertTo-TimeAgo {
<#
  .synopsis
  Returns the humanized relative difference between the provided date and right now.
  .parameter date
  The date to convert.
  .example
  > convertto-timeago $now.AddMinutes(-10)
  10 minutes ago
  .example
  > convertto-timeago $now.AddSeconds(45)
  45 seconds from now
#>
  param(
    [parameter(helpmessage = 'A datetime in the future or past for which to get a relative comparison.')]
    [validatenotnull()]
    [datetime] $date
  )

  # convert to timespan
  $now     = [datetime]::UtcNow
  $dateutc = $date.ToUniversalTime()
  $span    = $now.Subtract($dateutc).Duration()

  # timespan in each unit of measurement
  $millis = $span.TotalMilliseconds
  $weeks  = $millis / $WEEK
  $months = $millis / $MONTH
  $years  = $millis / $YEAR

  # determine phrase to use in this comparison, and the modifier if needed
  $phrase   = if ($now.Ticks -gt $dateutc.Ticks) { 'ago' } else { 'from now' }

  # compare every unit against the value in millis, and render the result accordingly
  if     ($millis -eq 0)       { "right now" }
  elseif ($millis -lt $SECOND) { "$(($span.TotalMilliseconds).ToString("#")) ms $phrase" }
  elseif ($millis -lt $MINUTE) { "$(($span.TotalSeconds).ToString("#")) seconds $phrase" }
  elseif ($millis -lt $HOUR)   { "$(($span.TotalMinutes).ToString("#")) minutes $phrase" }
  elseif ($millis -lt $DAY)    { "$(($span.TotalHours).ToString("#")) hours $phrase" }
  elseif ($millis -lt $WEEK)   { "$(($span.TotalDays).ToString("#")) days $phrase" }
  elseif ($millis -lt $MONTH)  { "$(($weeks).ToString("#")) weeks $phrase" }
  elseif ($millis -lt $YEAR)   { "$(($months).ToString("#")) months $phrase" }
  else                         { "$(($years).ToString("#")) months $phrase" }
}

function ConvertFrom-ISO8601(
  [parameter(valueFromPipeline=$true)]
  [string]$date = { throw "Invalid argument, string cannot be null or empty!" },
  [switch]$zulu = $false) {
<#
  .SYNOPSIS
  Converts a ISO8601 date string value to System.DateTime.
  .PARAMETER date
  The date string to parse.
  .PARAMETER zulu
  This switch determines whether or not to parse this date as ISO8601Z formatted.
  .INPUTS
  None
  .OUTPUTS
  DateTime
  .EXAMPLE
  > convertfrom-iso8601 '2014-02-01T12:00:00'
#>
  trap { trace-error $error -default 'Invalid date format!'; return; }

  $format_info  = $DATETIME_FORMAT_INFO
  $format_style = $DATETIME_STYLE
  # Attempt the extended format first, then fallback to the non-extended, otherwise fail
  try {
    [DateTime]::ParseExact($date, $DATETIME_FORMATS['iso8601_ext'], $format_info, $format_style)
  } catch [System.FormatException] {
    [DateTime]::ParseExact($date, $DATETIME_FORMATS['iso_8601'], $format_info, $format_style)
  }
}

function ConvertTo-ISO8601(
  [parameter(valueFromPipeline=$true)]
  [datetime] $date,
  [switch] $zulu = $false) {
<#
  .SYNOPSIS
  Converts a System.DateTime to an ISO8601/ISO8601Z string.
  .PARAMETER date
  The date to convert
  .PARAMETER zulu
  This switch determines whether or not to write this date in ISO8601Z format.
  .INPUTS
  None
  .OUTPUTS
  String
#>
  # Make sure the date is converted to local/zulu depending on flags
  $format = $DATETIME_FORMATS['iso8601_ext']
  if ($zulu) {
    $date.ToUniversalTime().ToString($format)
  } else {
    $date.ToString($format)
  }
}
