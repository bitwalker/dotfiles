# See → fishshell.com/docs/current/commands.html#complete

function __fish_mix_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'mix' ]
    return 0
  end
  return 1
end

# fish completion for mix
function __fish_mix_tasks
    mix help ^/dev/null | awk '{
        if ($1 == "mix") {
            if ($3 == "#") {
                print $2"\t"substr($0, index($0, $4))
            } else {
                print $1"\t"substr($0, index($0, $3))
            }
        }
    }'
end

complete -c mix -n '__fish_mix_needs_command' -f -a '(__fish_mix_tasks)'
