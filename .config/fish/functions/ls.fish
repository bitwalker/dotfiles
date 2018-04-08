function ls --description 'List contents of directory'
	command ls \
        -g \
        -s \
        --no-group \
        --dereference-command-line \
        --classify \
        --escape \
        --almost-all \
        --format=long \
        --time-style=iso \
        --color=auto \
        --group-directories-first \
        --human-readable \
        --indicator-style=slash $argv
end

function lss --description 'List contents of directory concisely'
    command ls \
      -g \
      -s \
      --no-group \
      --dereference-command-line \
      --escape \
      --almost-all \
      --format=horizontal \
      --time-style=iso \
      --color=auto \
      --group-directories-first \
      --human-readable \
      --indicator-style=slash $argv
end

function lsa --description 'List contents of directory verbosely'
    command ls \
        -l \
        -s \
        --dereference-command-line \
        --classify \
        --escape \
        --almost-all \
        --format=long \
        --time-style=long-iso \
        --color=auto \
        --group-directories-first \
        --human-readable \
        --indicator-style=slash $argv
end
