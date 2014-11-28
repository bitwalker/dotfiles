<#
  .synopsis
  This script includes the most minimal set of functionality that is used across
  just about every module. It contains the items that are typically present in
  the header of all my custom modules, such as error trapping, importing modules/scripts,
  etc. It is intended to be the very first thing you see in a module definition.
#>

function let([hashtable]$vars = @{}, [scriptblock]$scope) {
<#
  .synopsis
  Like the let keyword in ML-style languages, or in some lisps, this
  function binds immutable values to names within a given scope. They
  will shadow any vars of the same name from an outer scope, and are not
  available beyond the script block provided.
  .parameter vars
  A hashmap of names to values. Once bound, these values are immutable, and an
  error will be thrown if you attempt to change their values.
  .parameter scope
  A script block in which the bound values will be available for use. In theory,
  you could use set-variable within the block to make a mutable copy of the values
  bound here, but that would kind of defeat the point, so if you find yourself about
  to do that, step back and think about what went wrong.
  .example
  A totally useless example of how best to use this, but it should give you some ideas:

  > get-service | `
    where { $_.name -like 'mycompany.*' } | `
    let { action = "$_.name is $_.status"; date = get-date; } {
      write-host "${converto-iso8601 $date} - $action"
    }
#>
}

function get-local([string]$var, [alias('d', 'def')][switch]$definition = $false) {
<#
  .synopsis
  Gets a local variable of the given name from the current scope.
  In case it's not clear exactly what 'local' scope I mean, I'm specifically
  referring to local variables in the same scope as the caller of `get-local`.
  It is roughly equivalent to:

      `get-variable -name thing -scope 0 -valueonly -erroraction silentlycontinue`

  Or if you provide the -def flag:

      `get-variable -name thing -scope 0 -erroraction silentlycontinue`

  ErrorAction is set to SilentlyContinue to imply that the value of the given name is
  considered null when the context is limited to the local scope. It does not mean that
  a variable of that name does not exist, or is null in the containing scope (i.e. a global),
  but the idea here is that we can enforce scoping rules that powershell otherwise would not.
  .example
  Standard usage, fetching the value of the named variable:

  > $a = @(1,2,3)
  > get-variable a
  1
  2
  3
  .example
  When you provide the -definition flag, a PSVariable is returned instead of the value:

  > $a = @(1,2,3)
  > get-variable a -def | format-list *
  Name        : a
  Description :
  Value       : {1, 2, 3}
  Visibility  : Public
  Module      :
  ModuleName  :
  Options     : None
  Attributes  : {}
  .example
  If you provide a wildcard which matches more than one variable, the values are written to
  output consecutively, which can lead to some interesting output:

  > $a = @(1,2); $a2 = @(3,4)
  > get-variable a*
  1
  2
  3
  4

  While it may look like the two arrays were concatenated, but what actually happened is that
  the output received contained 3 objects, $a, $a2, and $args, and each one was iterated over
  as it was received via the pipeline. Don't be fooled by this!
#>
  if ($definition) {
    get-variable $var -scope 1
  } else {
    get-variable $var -scope 1 -valueonly
  }
}

function require {
<#
  .synopsis
  Used similarly to import-module, but is more informative
  when failures occur, and executes an optional callback on
  successful import.
  .parameter module
  The path to the module that is required
  .parameter onerror
  Either a function or a script block to execute when an error occurs.
  It will receive an ErrorRecord as it's sole parameter. If nothing is
  provided, an error will be written via write-error, and the script
  will exit.
#>
  param(
    [parameter(mandatory=$true, valuefrompipeline=$true)]
    [validatenotnull()]
    [string[]] $modules,
    []
    [string[]] $only,
    [alias('nc')]
    [switch]$noconflict,
    [alias('e')]
    $onerror = $null
  )
}