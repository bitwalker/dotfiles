function __prompt_section_dir -d "Display the current (truncated) working directory"
	__prompt_util_set_default PROMPT_DIR_SHOW true
	__prompt_util_set_default PROMPT_DIR_PREFIX "in "
	__prompt_util_set_default PROMPT_DIR_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_DIR_TRUNC 3
	__prompt_util_set_default PROMPT_DIR_TRUNC_REPO true
	__prompt_util_set_default PROMPT_DIR_COLOR $fish_color_cwd

	# Write Permissions lock symbol
	__prompt_util_set_default PROMPT_DIR_LOCK_SHOW true
	__prompt_util_set_default PROMPT_DIR_LOCK_SYMBOL ""
	__prompt_util_set_default PROMPT_DIR_LOCK_COLOR $fish_color_error

	[ $PROMPT_DIR_SHOW = false ]; and return

	set -l dir
	set -l tmp
	set -l git_root (command git rev-parse --show-toplevel 2>/dev/null)

	if test "$PROMPT_DIR_TRUNC_REPO" = "true" -a -n "$git_root"
		# Resolve to physical PWD instead of logical
		set -l resolvedPWD (pwd -P 2>/dev/null; or pwd)
		# Treat repo root as top level directory
		set tmp (string replace $git_root (basename $git_root) $resolvedPWD)
	else
		set -l realhome ~
		set tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $PWD)
	end

	# Truncate the path to have a limited number of dirs
	set dir (__prompt_util_truncate_dir $tmp $PROMPT_DIR_TRUNC)

	if [ $PROMPT_DIR_LOCK_SHOW = true -a ! -w . ]
		set DIR_LOCK_SYMBOL (set_color $PROMPT_DIR_LOCK_COLOR)" $PROMPT_DIR_LOCK_SYMBOL"(set_color --bold)
	end

	__prompt_lib_section \
		$PROMPT_DIR_COLOR \
		$PROMPT_DIR_PREFIX \
		$dir \
		"$DIR_LOCK_SYMBOL""$PROMPT_DIR_SUFFIX"
end
