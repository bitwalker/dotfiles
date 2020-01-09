function __prompt_section_git -d "Display the git branch and status"
	__prompt_util_set_default PROMPT_GIT_SHOW true
	__prompt_util_set_default PROMPT_GIT_PREFIX "on "
	__prompt_util_set_default PROMPT_GIT_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_GIT_SYMBOL " "

	# ------------------------------------------------------------------------------
	# Section
	# ------------------------------------------------------------------------------

	# Show both git branch and git status:
	#   prompt_git_branch
	#   prompt_git_status
    
    set -l project_name (basename (pwd))

	[ $PROMPT_GIT_SHOW = false ]; and return

    # Make sure this is a git repo
    if not test -d .git; and not command git rev-parse --git-dir >/dev/null 2>&1
        return
    end

	set -l git_branch (__prompt_section_git_branch)

    # Special handling for the LLVM repository since it is so huge,
    # we only show the current branch, nothing more
    if test "$project_name" = "llvm-project"
        set -l git_status ""
    else
        set -l git_status (__prompt_section_git_status)
    end


	__prompt_lib_section \
		fff \
		$PROMPT_GIT_PREFIX \
		"$git_branch$git_status" \
		$PROMPT_GIT_SUFFIX
end
