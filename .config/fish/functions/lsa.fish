function lsa -d "List contents of a directory with extra detail"
    # Invoke our tree function if tree is installed
    if type --no-functions tree
        tree -D $argv
    else
        ls --format=long $argv
    end
end
