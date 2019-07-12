function __prompt_section_rust -d "Display the current Rust version"
	__prompt_util_set_default PROMPT_RUST_SHOW true
	__prompt_util_set_default PROMPT_RUST_SHOW_TOOLCHAIN false
	__prompt_util_set_default PROMPT_RUST_PREFIX ""
	__prompt_util_set_default PROMPT_RUST_SUFFIX ""
    __prompt_util_set_default PROMPT_RUST_SYMBOL "rust "
	__prompt_util_set_default PROMPT_RUST_COLOR red

	# Show current version of Rust
	[ $PROMPT_RUST_SHOW = false ]; and return

	# Ensure the rustc command is available
	type -q rustc; or return

    if type -q cargo
        if not cargo pkgid --quiet --offline >/dev/null
            return
        end
    else if not test -f Cargo.toml -o (count *.rs) -gt 0
        return
	end

    # Example: rustc 1.37.0-nightly (03ee55bb1 2019-06-01)
	set -l rust_version_parts (rustc --version | string split ' ')
    set -l rust_version_num_parts (string split '-' $rust_version_parts[2])
    # e.g. 1.37.0
    set -l rust_version $rust_version_num_parts[1]
    set -l rust_version_info $rust_version

	if test $PROMPT_RUST_SHOW_TOOLCHAIN != false 
        and type -q rustup
        # Add toolchain information
        set -l rust_toolchain_verbose (rustup show active-toolchain | string split ' ')[1]
        set -l rust_toolchain_parts (string split '-' $rust_toolchain_verbose)
        set -l rust_toolchain_type $rust_toolchain_parts[1]
        set -l rust_toolchain (string join '-' $rust_toolchain_parts[5..-1])
        set rust_version_info $rust_version'-'$rust_toolchain_type':'$rust_toolchain
    else
        # Else display simplified toolchain info in the version, e.g. "nightly"
        set -l rust_toolchain $rust_version_num_parts[2]
        set rust_version_info $rust_version'-'$rust_toolchain
	end

	__prompt_lib_section \
		$PROMPT_RUST_COLOR \
		$PROMPT_RUST_PREFIX \
		["$PROMPT_RUST_SYMBOL"v"$rust_version_info"] \
		$PROMPT_RUST_SUFFIX
end
