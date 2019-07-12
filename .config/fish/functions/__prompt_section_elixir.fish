function __prompt_section_elixir -d "Show current version of Elixir"
	__prompt_util_set_default PROMPT_ELIXIR_SHOW true
	__prompt_util_set_default PROMPT_ELIXIR_PREFIX $PROMPT_DEFAULT_PREFIX
	__prompt_util_set_default PROMPT_ELIXIR_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_ELIXIR_SYMBOL "💧 "
	__prompt_util_set_default PROMPT_ELIXIR_DEFAULT_VERSION $PROMPT_ELIXIR_DEFAULT_VERSION
	__prompt_util_set_default PROMPT_ELIXIR_COLOR magenta

	# Check if that user wants to show elixir version
	[ $PROMPT_ELIXIR_SHOW = false ]; and return

	# Show versions only for Elixir-specific folders
	if not test -f mix.exs \
		-o (count *.ex) -gt 0 \
		-o (count *.exs) -gt 0
		return
	end

	set -l elixir_version

    if type -q asdf
        set -l asdf_version (asdf current elixir)
        # We may have asdf, but not be managing Elixir via asdf
        if test $status = 0
            set elixir_version (string split ' ' $asdf_version)[1]
        else if type -q elixir
            set -l elixir_version_header (elixir --version 2>/dev/null)[3]
            set elixir_version (string split ' ' $elixir_version_header)[2]
        else
            return
        end
	else if type -q elixir
        set -l elixir_version_header (elixir --version 2>/dev/null)[3]
        set elixir_version (string split ' ' $elixir_version_header)[2]
	else
		return
	end

	[ -z "$elixir_version" -o "$elixir_version" = "system" ]; and return

	# Add 'v' before elixir version that starts with a number
	if test -n (echo (string match -r "^[0-9].+\$" "$elixir_version"))
		set elixir_version "v$elixir_version"
	end

	__prompt_lib_section \
		$PROMPT_ELIXIR_COLOR \
		$PROMPT_ELIXIR_PREFIX \
		"$PROMPT_ELIXIR_SYMBOL""$elixir_version" \
		$PROMPT_ELIXIR_SUFFIX
end
