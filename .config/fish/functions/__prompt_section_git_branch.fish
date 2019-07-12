function __prompt_section_git_branch -d "Format the displayed branch name"
	__prompt_util_set_default PROMPT_GIT_BRANCH_SHOW true
	__prompt_util_set_default PROMPT_GIT_BRANCH_PREFIX $PROMPT_GIT_SYMBOL
	__prompt_util_set_default PROMPT_GIT_BRANCH_SUFFIX ""
	__prompt_util_set_default PROMPT_GIT_BRANCH_COLOR magenta

	[ $PROMPT_GIT_BRANCH_SHOW = false ]; and return

	set -l git_branch (__prompt_util_git_branch)

	[ -z $git_branch ]; and return

	__prompt_lib_section \
		$PROMPT_GIT_BRANCH_COLOR \
		$PROMPT_GIT_BRANCH_PREFIX$git_branch$PROMPT_GIT_BRANCH_SUFFIX
end
