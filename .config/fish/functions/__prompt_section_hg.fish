function __prompt_section_hg -d "Display the Mercurial branch and status"
	__prompt_util_set_default PROMPT_HG_SHOW true
	__prompt_util_set_default PROMPT_HG_PREFIX "on "
	__prompt_util_set_default PROMPT_HG_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_HG_SYMBOL " "

	# ------------------------------------------------------------------------------
	# Section
	# ------------------------------------------------------------------------------

	# Show both hg branch and hg status:
	#   prompt_hg_branch
	#   prompt_hg_status

	[ $PROMPT_HG_SHOW = false ]; and return

	set -l hg_branch (__prompt_section_hg_branch)

	[ -z $hg_branch ]; and return

	set -l hg_status (__prompt_section_hg_status)

	__prompt_lib_section \
		fff \
		$PROMPT_HG_PREFIX \
		"$hg_branch$hg_status" \
		$PROMPT_HG_SUFFIX
end
