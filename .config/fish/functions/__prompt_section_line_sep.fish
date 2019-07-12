function __prompt_section_line_sep -d "Separate the prompt into two lines"
	__prompt_util_set_default PROMPT_SEPARATE_LINE true

	if test "$PROMPT_SEPARATE_LINE" = "true"
		echo -e -n \n
	end
end
