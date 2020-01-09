function __prompt_section_time -d "Display the current time!"
	__prompt_util_set_default PROMPT_TIME_SHOW false
	__prompt_util_set_default PROMPT_DATE_SHOW false
	__prompt_util_set_default PROMPT_TIME_PREFIX "at "
	__prompt_util_set_default PROMPT_TIME_SUFFIX $SPACEFISH_PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_TIME_FORMAT false
	__prompt_util_set_default PROMPT_TIME_12HR false
	__prompt_util_set_default PROMPT_TIME_COLOR $fish_color_autosuggestion

	# ------------------------------------------------------------------------------
	# Section
	# ------------------------------------------------------------------------------

	[ $PROMPT_TIME_SHOW = false ]; and return

	set -l time_str

	if test $PROMPT_DATE_SHOW = true
		set time_str (date '+%Y-%m-%d')" "
	end

	if not test $PROMPT_TIME_FORMAT = false
		set time_str "$time_str"(date '+'$PROMPT_TIME_FORMAT)
	else if test $PROMPT_TIME_12HR = true
		set time_str "$time_str"(date '+%I:%M:%S') # Fish doesn't seem to have date/time formatting.
	else
		set time_str "$time_str"(date '+%H:%M:%S')
	end

	__prompt_lib_section \
		$PROMPT_TIME_COLOR \
		$PROMPT_TIME_PREFIX \
		$time_str \
		$PROMPT_TIME_SUFFIX
end
