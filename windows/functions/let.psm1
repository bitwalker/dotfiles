<#
  .synopsis
  In a query expression, such as `get-service | where { $_.status -eq 'running' }`, it is
  sometimes useful to store the result of a sub-expression in order to use it in subsequent
  clauses. `let` allows you to do this. It creates a set of bindings, each a mapping between
  a name and an expression e.g. `is_running = $_.status -eq 'running'`, which will be made available
  during execution of the next block. These bindings are immutable, so once bound by let, they
  cannot be changed or used to store another variable.

  **NOTE** The bindings created by `let` in it's head clause are only available within the body clause of
  the same `let` expression. If you try to access them outside that scope, an error will be thrown.

  .notes
  So, some thoughts on `let`, it's benefits, and it's implications in the ever-present functional vs
  imperative language philosophy war.

  So one of the benefits of `let` is that it's bindings are guaranteed not to pollute any parent scope,
  and vice versa, as changes to variables made in the parent scope cannot affect the variables in the
  body of the let. In addition, the use of immutability guarantees that you are working with the inputs
  in terms tranformations, rather than as mutable variables - which forces you to express your code
  declaritively (i.e. select, where, filter, order by, group by, etc), rather than imperatively via the
  mutation of some stateful object. While that may not sound that useful, consider if you will a scenario,
  where something in your code has gone wrong, what is involved with tracking down the source?

  With the functional transformation approach, each component of the whole is effectively a function which
  takes some inputs, transforms them, and then produces a single output which can then act as the input
  to yet another component. This property of functional programming makes it really trivial to plug new
  components into your stack, say a debugger, or a tracing module for instance - you could even stub out
  components pulling in variable data (think event sourcing a twitter stream, or the windows event log) so
  that they instead produce an idempotent set of data so that you can focus on validating the logic of the
  program.,  the beauty of it is that you can do all of this at every level of your app, and as long as you
  follow one important rule - what goes in must go out - your code will chug along without knowing anything
  has changed.

  With the stateful transformation approach, the ease with which you can inject tracing code, or hook in a
  debugger without affecting the timing of state changes, is limited to a few extension points where you
  probably already have those things set up, or even if you don't, won't necessarily help you if the problem
  is two layers deeper than you can safely go without changing the semantics of your code. Because you can't
  rely on one set of inputs *always* producing the same output, how do you verify that the result you get each
  time is valid? In C# you can escape from this trap quite handily with the use of dependency injection, but
  very rarely do people bother breaking things down to the function level - and if you are digging around trying
  to find where that subtle bug came from, not being able to 'verify the pipeline', or see that changes you make one
  one side of a component produces the correct result on the other

  Ultimately my point of view is going to come off as exceedingly harsh against stateful architecture, but I
  believe that's due primarily to how greatly the functional approach can improve the ease of understanding
  your code, and make extending/debugging/refactoring and maintaining that code actually pleasant in comparison.
  Mutability is like a disease that infects everything, incubates for long periods of time, and then strikes
  with vengeance at it's hosts. Avoid using it if you care about the quality of your code in the long term.

  So yeah, why was I here again, oh yeah `let`. It's good shit, use it.
#>

function local:write-expressionError([string]$expression) {
write-host `
@"

Invalid let expression: $expression
Syntax is: <name> = <value | expression> [;]
Examples:
  value assignment:   limit  = 10; foo = 'bar'
  simple expression:  count  = `$_.count
  complex expression: active = `$_ | where { `$_.status = 'active' }

"@ -foregroundcolor red
}

function let {
<#
  .synopsis
  Creates immutable bindings scoped to a single block. See module docs for more info.
  .parameter bindings
  A block using a special syntax for binding expressions to names. See examples.
  .parameter block
  The block of code to execute with the bindings in scope
  .parameter objs
  Either data pipelined to this function, or provided as additional arguments after $bindings
  and $block. $block will be executed once per item in this collection, similar to `where`.
#>
  [cmdletbinding()]
  param(
    [parameter(position=0,mandatory=$true)]
    [scriptblock]
    $private:bindings,
    [parameter(position=1,mandatory=$true)]
    [scriptblock]
    $private:block,
    [parameter(valueFromPipeline=$true,valueFromRemainingArguments=$true)]
    [psobject[]]
    $private:objs = $null
  )

  <#
  Collect the entire pipeline so that the $binding block has access to it.
  To keep the semantics of piping however, when $block is invoked later, it is
  done inside of a foreach, where each iteration writes the invocation result via
  write-output. If you were to pipe an array into `let` which writes each item to the
  console, and pipe that into a foreach which does the same, it will look like this:

  > @(1,2,3) | let { format = 'sending {0}' } | { write-host ($format -f $_) } | % { write-host "received $_" }
  sent 1
  received 1
  sent 2
  received 2
  sent 3
  received 3

  So while `let` does consume the entire pipeline in order to make the current state of the piped
  object available to the binding block, it does not hold up the works once it actually has the
  bindings ready, as it will immediately start writing output as the pipeline is re-processed.
  #>
  begin   { $pipeline = @() }
  process { $pipeline = $pipeline + $_ }

  end {
    <#
    While script blocks allow param/begin/process/end blocks, `let`
    only allow assignments in the binding block, which means that powershell
    will treat it like a single end block. Therefore, we only bother pulling
    statements from the end block of the provided scriptblock
    #>
    $private:block_vars  = $bindings.Ast.EndBlock.Statements | `
        foreach { $_.PipelineElements } | `
        foreach {
          # Validate expression and break out of everything if invalid
          $private:elements                  = $_.CommandElements
          $private:is_valid_element          = $_.GetType() -eq [System.Management.Automation.Language.CommandAst]
          $private:has_valid_component_count = $elements.count -eq 3
          $private:is_valid_name             = (-not $elements[0].Extent.Text.StartsWith('$'))
          $private:is_valid_assignment       = ($elements[0].StringConstantType -eq 'BareWord' -and $elements[1].Extent.Text -eq '=')
          if (-not ($has_valid_component_count -and $is_valid_assignment -and $is_valid_name)) {
            # This represents the text of the current expression being parsed,
            # so that `let` can notify the user exactly which expression was invalid
            write-expressionError $_.Extent.Text
            break
          }
          <#
          Grab the variable name and value expression from the current expression, and
          invoke the value expression in a new context containing the pipeline, so that
          `let` can be generous about how assignments are made. The only restriction here is
          no stateful assignments, i.e. mutation, since `let` is enforcing immutability, i.e.
          "X is Y, forever" rather than the mutable assertion that "X is equivalent to Y, for now".
          In functional programming with immutable values, there is no distinction between X
          and Y assuming they represent the same value. If X is Y, and X is 10, then Y must be 10.
          #>
          $private:name  = $elements[0].Extent.Text
          $private:val   = $elements[2].Extent.Text
          $private:get_val_block = [scriptblock]::create($val)
          $private:context_fns    = @{}
          $private:context_vars   = @(new-object 'PSVariable' @('_', $pipeline))
          # Just for future reference, the signature of InvokeWithContext is:
          # InvokeWithContext(Dictionary<string, ScripBlock> functionsToDefine, PSVarible[] variablesToDefine, object[] args = null)
          $private:assigned_value = $get_val_block.InvokeWithContext($context_fns, $context_vars)
          # Create the variable for the block context, set it to be privately-scoped and readonly (immutable)
          $var = new-object 'PSVariable' @($name, $assigned_value)
          $var.Visibility = [System.Management.Automation.SessionStateEntryVisibility] 'Private'
          $var.Options    = [System.Management.Automation.ScopedItemOptions] 'Private, ReadOnly'
          $var
    }

    # Emulate executing $block as a pipelined scriptblock by calling write-output
    # on the result of invoking the block with our constructed context, inside a foreach loop.
    foreach ($piped in $pipeline) {
      $pipe_vars = @(new-object 'PSVariable' @('_', $piped)) + $block_vars
      write-output ($block.InvokeWithContext(@{}, $pipe_vars))
    }

    # Explicitly return to keep from writing internal state to the output stream
    return
  }
}