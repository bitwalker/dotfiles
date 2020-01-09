function __prompt_section_exit_code -d "Shows the exit code from the previous command."
	__prompt_util_set_default PROMPT_EXIT_CODE_SHOW false
	__prompt_util_set_default PROMPT_EXIT_CODE_PREFIX ""
	__prompt_util_set_default PROMPT_EXIT_CODE_SUFFIX " "
	__prompt_util_set_default PROMPT_EXIT_CODE_SYMBOL ✘
	__prompt_util_set_default PROMPT_EXIT_CODE_COLOR $fish_color_error

	[ $PROMPT_EXIT_CODE_SHOW = false ]; or test $sf_exit_code -eq 0; and return

	__prompt_lib_section \
		$PROMPT_EXIT_CODE_COLOR \
		$PROMPT_EXIT_CODE_PREFIX \
		"$PROMPT_EXIT_CODE_SYMBOL$sf_exit_code" \
		$PROMPT_EXIT_CODE_SUFFIX
end
