function __prompt_section_user -d "Display the username"
	# --------------------------------------------------------------------------
	# | PROMPT_USER_SHOW    | show username on local | show username on remote |
	# |---------------------+------------------------+-------------------------|
	# | false               | never                  | never                   |
	# | always              | always                 | always                  |
	# | true                | if needed              | always                  |
	# | needed              | if needed              | if needed               |
	# --------------------------------------------------------------------------

	__prompt_util_set_default PROMPT_USER_SHOW true
	__prompt_util_set_default PROMPT_USER_PREFIX "with "
	__prompt_util_set_default PROMPT_USER_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_USER_COLOR yellow
	__prompt_util_set_default PROMPT_USER_COLOR_ROOT red

	[ $PROMPT_USER_SHOW = false ]; and return

	if test "$PROMPT_USER_SHOW" = "always" \
	-o "$LOGNAME" != "$USER" \
	-o "$UID" = "0" \
	-o \( "$PROMPT_USER_SHOW" = "true" -a -n "$SSH_CONNECTION" \)

		set -l user_color
		if test "$USER" = "root"
			set user_color $PROMPT_USER_COLOR_ROOT
		else
			set user_color $PROMPT_USER_COLOR
		end

		__prompt_lib_section \
			$user_color \
			$PROMPT_USER_PREFIX \
			$USER \
			$PROMPT_USER_SUFFIX
	end
end
