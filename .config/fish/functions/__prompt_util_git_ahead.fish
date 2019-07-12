function __prompt_util_git_ahead
    __prompt_util_set_default PROMPT_GIT_STATUS_AHEAD ⇡
    __prompt_util_set_default PROMPT_GIT_STATUS_BEHIND ⇣
    __prompt_util_set_default PROMPT_GIT_STATUS_DIVERGED ⇕

    set -l count (command git rev-list --count --left-right "@{upstream}...HEAD")
    switch "$count"
        case "" "0"\t"0"
            # No upstream, or no outstanding commits
            echo ""
        case "*"\t"0"
            # There are upstream commits, but no local commits
            set -l incoming (echo "$count" | cut -f1)
            echo "$PROMPT_GIT_STATUS_BEHIND$incoming"
        case "0"\t"*"
            # There are local commits, but no upstream commits
            set -l outgoing (echo "$count" | cut -f2)
            echo "$PROMPT_GIT_STATUS_AHEAD$outgoing"
        case "*"
            # There are both local and upstream commits
            set -l incoming (echo "$count" | cut -f1)
            set -l outgoing (echo "$count" | cut -f2)
            echo "$PROMPT_GIT_STATUS_DIVERGED ($PROMPT_GIT_STATUS_BEHIND$incoming $PROMPT_GIT_STATUS_AHEAD$outgoing)"
    end
end

