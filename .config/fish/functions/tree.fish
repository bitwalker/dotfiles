function tree -d "Invokes tree with nice defaults"
    command tree -a \
        -l \
        -x \
        -L 1 \
        --dirsfirst \
        -q \
        -p \
        -h \
        -F \
        -C \
        $argv
end
