# SYNOPSIS
#   autoload <path>...
#   autoload -e <path>...
#
# OVERVIEW
#   Manipulate autoloading path components.
#
#   If called without options, the paths passed as arguments are added to
#   $fish_function_path. All paths ending with `completions` are correctly
#   added to $fish_complete_path. Returns 0 if one or more paths exist. If all
#   paths are missing, returns != 0.
#
#   When called with -e, the paths passed as arguments are removed from
#   $fish_function_path. All arguments ending with `completions` are correctly
#   removed from $fish_complete_path. Returns 0 if one or more paths erased. If
#   no paths were erased, returns != 0.
function autoload -d "Manipulate autoloading path components"
    set -l paths $argv
    set -l erase false

    switch "$argv[1]"
        case '-e' '--erase'
            set erase true

            if test (count $argv) -ge 2
                set paths $argv[2..-1]
            else
                echo "usage: autoload $argv[1] <path>..." 1>&2
                return 1
            end
        case "-*" "--*"
            echo "autoload: invalid option $argv[1]"
            return 1
    end

    set -l success false
    for path in $paths
        not test -d "$path"; and continue

        set -l dest fish_function_path

        if test (basename "$path") = "completions"
            set dest fish_complete_path
        end

        if $erase
            if set -l index (contains -i -- $path $$dest)
                set success true
                set -e {$dest}[$index]
            end
        else
            set success true
            contains -- "$path" $$dest; and continue
            # Always protect the core functions by keeping them first
            set $dest {$dest}[1] "$path" {$dest}[2..-1]
        end
    end

    test $success = true
end
