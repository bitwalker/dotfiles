function __prompt_util_hg_branch -d "Display the current Mercurial branch name"
    if ! __prompt_util_hg_find_root 2>&1
        return 1
    end

    # If we reach here, this is a Mecurial-controlled directory
    # set -l commit_hash (hexdump -n 4 -e '1/1 "%02x"' "$HG_ROOT/dirstate" | cut -c-7)

    # Get the branch name, then check if we have a bookmark active, in which case print that instead
    set -l branch_name (cat "$HG_ROOT/branch" 2>/dev/null; or hg branch --color=never --quiet --noninteractive)
    if test -f "$HG_ROOT/bookmarks.current"
        set -l bookmark_name (cat "$HG_ROOT/bookmarks.current" 2>/dev/null; or hg bookmarks --quiet --list .)
        echo "$branch_name at bookmark $bookmark_name"
    else
        echo "$branch_name"
    end
end
