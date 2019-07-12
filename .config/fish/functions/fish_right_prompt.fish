function fish_right_prompt
    __prompt_util_set_default RPROMPT_ORDER ""

    [ -n "$RPROMPT_ORDER" ]; or return

    for i in $RPROMPT_ORDER
        eval __prompt_section_$i
    end
    set_color normal
end
