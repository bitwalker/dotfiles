function ls -d 'List contents of directory'
    command ls \
        --almost-all \
        --human-readable \
        --size \
        --time-style=iso \
        -v \
        --width=80 \
        --color=auto \
        --classify \
        --group-directories-first \
        --dereference-command-line \
        --escape \
        $argv
end
