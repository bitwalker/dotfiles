<#
  .SYNOPSIS
  Useful helper functions for logging to the console
#>

<#
  Valid Color Names
  -------------------------
  Black         DarkBlue
  DarkGreen     DarkCyan
  DarkRed       DarkMagenta
  DarkYellow    Gray
  DarkGray      Blue
  Green         Cyan
  Red           Magenta
  Yellow        White
#>
$colors = @{
  'debug'   = 'white';
  'info'    = 'blue';
  'success' = 'green'
  'alert'   = 'cyan';
  'warn'    = 'yellow';
  'error'   = 'red'
}

function get-callerinfo {
  trap { continue; }
  $frames = (get-pscallstack | select-object command,location,arguments)
  $caller = $frames[2]
  "$($caller.location), $($caller.command) $($caller.arguments)"
}

function show-log($message, $color = "white") {
  write-host $message -foregroundcolor $color
}

function show-debug($message) {
  show-log "DEBUG: $message" $colors['debug']
}

function show-info($message) {
  show-log $message $colors['info']
}

function show-success($message) {
  show-log $message $colors['success']
}

function show-alert($message) {
  show-log $message $colors['alert']
}

function show-warning($message) {
  show-log $message $colors['warn']
}

function show-error($message) {
  show-log $message $colors['error']
}

function show-object([parameter(ValueFromPipeline=$true)] $obj) {
  trap { continue; }

  # Show caller information
  $caller = get-callerinfo
  show-warning $caller
  # Show object details
  $obj | format-list *
  # Draw a line to demarcate the end of this inspect operation
  show-warning ('-' * 20)
}

function trace-error($errors, [string]$default = 'A non-terminating error occurred, but contained no details.') {
<#
  .SYNOPSIS
  Pretty prints errors contained in $error
  .EXAMPLE
  Use in conjunction with trap:

    function get-things {
      trap { trace-error $error -default 'Something broke'; return; }
      ...
    }

#>
  if ($errors -ne $null -and $errors.Count -gt 0) {
    $err           = $errors[0]
    $location      = $err.InvocationInfo.PositionMessage
    $exception     = $err.Exception
    $exceptionType = $exception.GetType().Name
    $message       = $exception.Message

    show-error "$exceptionType : $message"
    show-error $location
  } else {
    $caller = get-callerinfo
    show-error "$default"
    show-error $caller
  }
}
