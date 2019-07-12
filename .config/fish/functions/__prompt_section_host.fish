function __prompt_section_host -d "Display the hostname of the current shell, which may be a local or remote host (e.g. over SSH)"
    # If there is an ssh connections, current machine name.
	__prompt_util_set_default PROMPT_HOST_SHOW true
	__prompt_util_set_default PROMPT_HOST_PREFIX "at "
	__prompt_util_set_default PROMPT_HOST_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_HOST_COLOR blue
	__prompt_util_set_default PROMPT_HOST_COLOR_SSH green

	[ "$PROMPT_HOST_SHOW" = false ]; and return

	if test "$PROMPT_HOST_SHOW" = "always"; or set -q SSH_CONNECTION;

		# Determination of what color should be used
		set -l host_color
		if set -q SSH_CONNECTION;
			set host_color $PROMPT_HOST_COLOR_SSH
		else
			set host_color $PROMPT_HOST_COLOR
		end

		__prompt_lib_section \
			$host_color \
			$PROMPT_HOST_PREFIX \
			(hostname) \
			$PROMPT_HOST_SUFFIX
    end
end
