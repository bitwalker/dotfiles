<#
  .SYNOPSIS
  Useful helper functions for logging to the console
#>

$colors = @{
  'debug'   = 'white';
  'info'    = 'blue';
  'success' = 'green'
  'warn'    = 'yellow';
  'error'   = 'red'
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

function show-warning($message) {
  show-log $message $colors['warn']
}

function show-error($message) {
  show-log $message $colors['error']
}

function trace-error($err) {
<#
  .SYNOPSIS
  Pretty prints errors contained in $error
  .EXAMPLE
  Use in conjunction with trap:

    function get-things {
      trap { trace-error $error[0]; return; }
      ...
    }

#>
  $location      = $err.InvocationInfo.PositionMessage
  $exception     = $err.Exception
  $exceptionType = $exception.GetType().Name
  $message       = $exception.Message

  show-error "$exceptionType : $message"
  show-error $location
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

function get-callerinfo {
  trap { continue; }
  $frames = (get-pscallstack | select-object command,location,arguments)
  $caller = $frames[2]
  "$($caller.location), $($caller.command) $($caller.arguments)"
}