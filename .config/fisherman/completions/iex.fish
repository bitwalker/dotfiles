# See → fishshell.com/docs/current/commands.html#complete
function __fish_iex_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'iex' ]
    return 0
  end
  return 1
end

# fish completion for iex
function __fish_iex_tasks
    iex -h 2>/dev/stdout | grep '^\s*\(-.*\)' | perl -n -e '/^\s+(--?\S+)\s+("(\S+)")?([\s|\xc2\xa0]+)(\S.*)$/ && print "$1\\t `$1 $2` $5\n"'
end

complete -c iex -n '__fish_iex_needs_command' -f -a '(__fish_iex_tasks)'
