function __prompt_section_jobs -d "Show icon, if there's a working jobs in the background."
	__prompt_util_set_default PROMPT_JOBS_SHOW true
	__prompt_util_set_default PROMPT_JOBS_PREFIX ""
	__prompt_util_set_default PROMPT_JOBS_SUFFIX " "
	__prompt_util_set_default PROMPT_JOBS_SYMBOL ✦
	__prompt_util_set_default PROMPT_JOBS_COLOR $fish_color_normal
	__prompt_util_set_default PROMPT_JOBS_AMOUNT_PREFIX ""
	__prompt_util_set_default PROMPT_JOBS_AMOUNT_SUFFIX ""
	__prompt_util_set_default PROMPT_JOBS_AMOUNT_THRESHOLD 1

	[ $PROMPT_JOBS_SHOW = false ]; and return

	set jobs_amount (jobs | wc -l | xargs) # Zsh had a much more complicated command.

	if test $jobs_amount -eq 0
		return
	end

	if test $jobs_amount -le $PROMPT_JOBS_AMOUNT_THRESHOLD
		set jobs_amount ''
		set PROMPT_JOBS_AMOUNT_PREFIX ''
		set PROMPT_JOBS_AMOUNT_SUFFIX ''
	end

	set PROMPT_JOBS_SECTION "$PROMPT_JOBS_SYMBOL$PROMPT_JOBS_AMOUNT_PREFIX$jobs_amount$PROMPT_JOBS_AMOUNT_SUFFIX"

	__prompt_lib_section \
		$PROMPT_JOBS_COLOR \
		$PROMPT_JOBS_PREFIX \
		$PROMPT_JOBS_SECTION \
		$PROMPT_JOBS_SUFFIX
end
