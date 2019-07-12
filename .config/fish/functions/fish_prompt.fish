function fish_prompt
    set -g last_status $status

    __prompt_util_set_default PROMPT_ADD_NEWLINE true
    __prompt_util_set_default PROMPT_FIRST_PREFIX_SHOW false
	__prompt_util_set_default PROMPT_PREFIXES_SHOW true
	__prompt_util_set_default PROMPT_SUFFIXES_SHOW true
	__prompt_util_set_default PROMPT_DEFAULT_PREFIX "via "
	__prompt_util_set_default PROMPT_DEFAULT_SUFFIX " "
	__prompt_util_set_default PROMPT_ORDER time user dir host git hg golang rust elixir venv line_sep exit_code char

    # Keep track of whether the prompt has already been opened
	set -g prompt_opened $PROMPT_FIRST_PREFIX_SHOW

	if test "$PROMPT_ADD_NEWLINE" = "true"
		echo
	end

	for i in $PROMPT_ORDER
		eval __prompt_section_$i
	end
	set_color normal
end
