<#
  .SYNOPSIS
  Contains various functional programming helpers.
#>

set-strictmode -version latest

function test-isEmpty([string]$str = "") {
<#
  .SYNOPSIS
  Returns true if the provided string is null or empty
  .PARAMETER str
  The string to check
#>
  [System.String]::IsNullOrWhitespace($str)
}


function new-tuple {
<#
  .SYNOPSIS
  Creates a new tuple
  .DESCRIPTION
  A tuple is a useful datastructure for storing small numbers
  of heterogenous elements. They can be treated like structs,
  with property access, value equality, and typical use cases.
  .PARAMETER elements
  A hashtable defining the mapping of names to values for this tuple.
  .INPUTS
  Optional. You can provide a pipelined hashtable in place of an argument.
  .OUTPUTS
  A new tuple
  .EXAMPLE
  > $user = new-tuple @{"id" = 1; "name" = "Paul"}
  > $user.id
  1
  > $user.name
  Paul
  .EXAMPLE
  > @{"id" = 1; "name" = "Paul"} | new-tuple | % { $_.name }
  Paul
#>
  param(
    [parameter(
      ValueFromPipeline=$true
    )]
    [hashtable]
    $elements = ${ throw "Cannot create an empty tuple!" }
  )

  return new-object PSObject -Property $elements
}

function select-n {
<#
  .SYNOPSIS
  Takes $count elements from the provided enumerable
  .PARAMETER xs
  Required. The source enumerable. Can be piped.
  .PARAMETER count
  Optional. The number of elements to take. If not provided,
  -1 is used as the default value, which represents infinity. If
  0 is provided, an empty collection of the same type is returned
  .INPUTS
  Optional. Can receive the enumerable via piped arguments.
  .OUTPUTS
  A copy of the source enumerable, but truncated past $count
  elements. The original collection is left untouched.
  .EXAMPLE
  > (1, 2, 3, 4) | select-n -count 2
  1
  2
#>
  param(
    [parameter(ValueFromPipeline=$true)]
    $xs = $null,
    [int]$count = -1
  )
  # If collection was piped, assign it to the input variable
  $xs = if ($input) { $input } else { ,$xs }
  # Get an enumerator for the collection
  $enumerator = $xs.GetEnumerator()
  # Return early if we can
  if ($count -lt 0) { return $enumerator }
  if ($count -eq 0) { return new-object -type $xs.GetType() 0 }

  $result = @()
  while ($enumerator.MoveNext() -and ($count-- -gt 0)) {
    $result = $result + @($enumerator.Current)
  }
  return $result
}

function select-zip($xs, $ys, [scriptblock]$zipper = $null) {
<#
  .SYNOPSIS
  Zips two sequences together
  .DESCRIPTION
  Combines two sequences into a one by selecting an element from both
  and combining the values via the provided zipper function, until one
  or both sequences are out of elements. Extra elements in the larger
  sequence are left behind.
  .PARAMETER xs
  Required. The first sequence
  .PARAMETER ys
  Required. The second sequence
  .PARAMETER zipper
  Optional. A script block which takes two parameters (an element from
  both sequences), and produces a new value representing the combination
  of those elements. If a zipper is not supplied, a default one which
  produces a tuple of the two elements is used.
  .INPUTS
  None.
  .OUTPUTS
  A single sequence containing the combined alements
  .EXAMPLE
  > $xs = (1, 2, 3)
  > $ys = (1, 2, 3, 4)
  > select-zip $xs $ys { param($x, $y) $x * $y }
  1
  4
  9
  .EXAMPLE
  > select-zip (1, 2, 3) (4, 5, 6) | % { "$_" }
  @{second=4; first=1}
  @{second=5; first=2}
  @{second=6; first=3}
#>
  # Produce a sequence of tuples if no zipper was supplied
  if ($zipper -eq $null) {
    $zipper = { param($x, $y) tuple(@{"first"=$x; "second"=$y}) }
  }
  $zipped = new-object System.Collections.ArrayList
  $xenum  = $xs.GetEnumerator()
  $yenum  = $ys.GetEnumerator()
  while ($xenum.MoveNext() -and $yenum.MoveNext()) {
    $xy = & $zipper $xenum.Current $yenum.Current
    $zipped.Add($xy) | out-null
  }

  return $zipped
}

