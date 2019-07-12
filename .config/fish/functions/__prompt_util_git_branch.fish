function __prompt_util_git_branch -d "Display the current branch name"
	if set -l ref (command git symbolic-ref --short --quiet HEAD 2>/dev/null)
        echo "$ref"
    else
        if set -l hash (command git rev-parse --short --verify HEAD 2>/dev/null)
            set -l described (command git describe --all --contains --always HEAD 2>/dev/null)
            echo "detached HEAD ($hash:$described)"
        else
            # Not a git repo
            return 1
        end
    end
end
