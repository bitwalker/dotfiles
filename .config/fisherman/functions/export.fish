# SYNOPSIS
#   export [options]
#
# USAGE
#   export NAME=VALUE
#

function export -d "export like bash does"
  set var1 (echo $argv | cut -f1 -d=)
  set var2 (echo $argv | cut -f2 -d=)
  set -x -g $var1 $var2
end

