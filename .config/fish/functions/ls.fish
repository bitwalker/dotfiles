function ls -d 'List contents of directory'
    if type --no-functions tree
        # Use tree when available
        tree $argv
    else
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
end
