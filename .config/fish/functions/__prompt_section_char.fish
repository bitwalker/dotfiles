function __prompt_section_char -d "Display the prompt character"
	__prompt_util_set_default PROMPT_CHAR_PREFIX ""
	__prompt_util_set_default PROMPT_CHAR_SUFFIX " "
    #__prompt_util_set_default PROMPT_CHAR_SYMBOL ➜
    #__prompt_util_set_default PROMPT_CHAR_SYMBOL »
    #__prompt_util_set_default PROMPT_CHAR_SYMBOL λ
    #__prompt_util_set_default PROMPT_CHAR_SYMBOL ⇒
    __prompt_util_set_default PROMPT_CHAR_SYMBOL ⋟
    #__prompt_util_set_default PROMPT_CHAR_SYMBOL ❯
    __prompt_util_set_default PROMPT_CHAR_ROOT_SYMBOL ‽
	__prompt_util_set_default PROMPT_CHAR_COLOR_SUCCESS green
	__prompt_util_set_default PROMPT_CHAR_COLOR_FAILURE $fish_color_error
	__prompt_util_set_default PROMPT_CHAR_ROOT_COLOR_SUCCESS orange
	__prompt_util_set_default PROMPT_CHAR_ROOT_COLOR_FAILURE $fish_color_error

    set -l user_id (id -u)

    # Color the symbol differently based on the success of the previous command
	set -l color
    set -l symbol

    if test $user_id -eq 0
        set symbol $PROMPT_CHAR_ROOT_SYMBOL

        if test $last_status -eq 0
            set color $PROMPT_CHAR_ROOT_COLOR_SUCCESS
        else
            set color $PROMPT_CHAR_ROOT_COLOR_FAILURE
        end
    else
        set symbol $PROMPT_CHAR_SYMBOL

        if test $last_status -eq 0
            set color $PROMPT_CHAR_COLOR_SUCCESS
        else
            set color $PROMPT_CHAR_COLOR_FAILURE
        end
	end

	__prompt_lib_section \
		$color \
		$PROMPT_CHAR_PREFIX \
		$symbol \
		$PROMPT_CHAR_SUFFIX
end
