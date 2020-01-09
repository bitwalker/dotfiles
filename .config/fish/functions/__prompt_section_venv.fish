function __prompt_section_venv -d "Show current virtual Python environment"
	__prompt_util_set_default PROMPT_VENV_SHOW true
	__prompt_util_set_default PROMPT_VENV_PREFIX $PROMPT_DEFAULT_PREFIX
	__prompt_util_set_default PROMPT_VENV_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_VENV_SYMBOL "·"
	__prompt_util_set_default PROMPT_VENV_GENERIC_NAMES virtualenv venv .venv
	__prompt_util_set_default PROMPT_VENV_COLOR $fish_color_cwd

	# Show venv python version
	 test $PROMPT_VENV_SHOW = false; and return

	# Check if the current directory running via Virtualenv
	test -n "$VIRTUAL_ENV"; or return

	set -l venv (basename $VIRTUAL_ENV)
	if contains $venv $PROMPT_VENV_GENERIC_NAMES
		set venv (basename (dirname $VIRTUAL_ENV))
	end

	__prompt_lib_section \
		$PROMPT_VENV_COLOR \
		$PROMPT_VENV_PREFIX \
		"$PROMPT_VENV_SYMBOL""$venv" \
		$PROMPT_VENV_SUFFIX
end
