function __prompt_section_erlang -d "Show current version of Erlang"
    __prompt_util_set_default PROMPT_ERLANG_SHOW true
    __prompt_util_set_default PROMPT_ERLANG_PREFIX $PROMPT_DEFAULT_PREFIX
    __prompt_util_set_default PROMPT_ERLANG_SUFFIX $PROMPT_DEFAULT_SUFFIX
    __prompt_util_set_default PROMPT_ERLANG_SYMBOL "erl "
    __prompt_util_set_default PROMPT_ERLANG_DEFAULT_VERSION $PROMPT_ERLANG_DEFAULT_VERSION
    __prompt_util_set_default PROMPT_ERLANG_COLOR red

    [ $PROMPT_ERLANG_SHOW = false ]; and return

    # Display for both Erlang and Elixir projects
    if not test -f rebar.config \
        -o test -f mix.exs \
        -o (count *.erl) -gt 0 \
        -o (count *.ex) -gt 0 \
        -o (count *.exs) -gt 0
        return
    end

    set -l erlang_version

    if type -q asdf
        set -l asdf_version (asdf current erlang)
        if test $status = 0
            set erlang_version (string split ' ' $asdf_version)[1]
        else if not type -q erl
            return
        end
    else if not type -q erl
        return
    end

    set -l erlang_root (erl -noshell -eval 'io:fwrite("~s~n", [code:root_dir()])' -s erlang halt)
    set erlang_version (string split '/')[-1]
    
    [ -z "$erlang_version" -o "$erlang_version" = "system" ]; and return

    __prompt_lib_section \
        $PROMPT_ERLANG_COLOR \
        $PROMPT_ERLANG_PREFIX \
        "$PROMPT_ERLANG_SYMBOL $erlang_version" \
        $PROMPT_ERLANG_SUFFIX
end
