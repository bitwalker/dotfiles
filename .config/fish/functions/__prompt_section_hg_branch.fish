function __prompt_section_hg_branch -d "Format the displayed Mercurial branch name"
	__prompt_util_set_default PROMPT_HG_BRANCH_SHOW true
	__prompt_util_set_default PROMPT_HG_BRANCH_PREFIX $PROMPT_HG_SYMBOL
	__prompt_util_set_default PROMPT_HG_BRANCH_SUFFIX ""
	__prompt_util_set_default PROMPT_HG_BRANCH_COLOR magenta

	[ $PROMPT_HG_BRANCH_SHOW = false ]; and return

	set -l hg_branch (__prompt_util_hg_branch)

	[ -z $hg_branch ]; and return

	__prompt_lib_section \
		$PROMPT_HG_BRANCH_COLOR \
		$PROMPT_HG_BRANCH_PREFIX$hg_branch$PROMPT_HG_BRANCH_SUFFIX
end
