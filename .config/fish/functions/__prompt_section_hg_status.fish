function __prompt_section_hg_status -d "Display the current hg status"
	__prompt_util_set_default PROMPT_HG_STATUS_SHOW true
    __prompt_util_set_default PROMPT_HG_STATUS_MAX_CHANGES 100
	__prompt_util_set_default PROMPT_HG_STATUS_PREFIX " ["
	__prompt_util_set_default PROMPT_HG_STATUS_SUFFIX ]
	__prompt_util_set_default PROMPT_HG_STATUS_CLEAN_SYMBOL ✓
	__prompt_util_set_default PROMPT_HG_STATUS_CLEAN_COLOR green
	__prompt_util_set_default PROMPT_HG_STATUS_DIRTY_COLOR red
	__prompt_util_set_default PROMPT_HG_STATUS_UNTRACKED \?
	__prompt_util_set_default PROMPT_HG_STATUS_ADDED +
	__prompt_util_set_default PROMPT_HG_STATUS_MODIFIED !
	__prompt_util_set_default PROMPT_HG_STATUS_DELETED ✘
	__prompt_util_set_default PROMPT_HG_STATUS_UNMERGED =
	__prompt_util_set_default PROMPT_HG_STATUS_AHEAD ⇡
	__prompt_util_set_default PROMPT_HG_STATUS_BEHIND ⇣
	__prompt_util_set_default PROMPT_HG_STATUS_DIVERGED ⇕
	__prompt_util_set_default PROMPT_HG_PROMPT_ORDER untracked added modified deleted unmerged

	[ $PROMPT_HG_STATUS_SHOW = false ]; and return

	set -l hg_status
	set -l is_ahead false
	set -l is_behind false
    set -l is_diverged false
    set -l untracked false
    set -l added false
    set -l modified false
    set -l deleted false
    set -l unmerged false

    set -l index (command hg status --color=never --noninteractive | string sub --start 1 --length 1)

	for i in $index
        switch $i
            case '?'
                if not $untracked
                    set untracked true
                    set hg_status untracked $hg_status
                end
            case '!'
                if not $deleted
                    set deleted true
                    set hg_status deleted $hg_status
                end
            case 'R'
                if not $deleted
                    set deleted true
                    set git_status deleted $git_status
                end
            case 'A'
                if not $added
                    set added true
                    set git_status added $git_status
                end
            case 'M'
                if not $modified
                    set modified true
                    set git_status modified $git_status
                end
            case '*'
                continue
		end
	end

    # TODO: Disabled until performance of hg is non-abysmal
	# Check whether the branch is ahead
    #if hg outgoing --quiet --noninteractive
    #    set is_ahead true
    #end

    # TODO: Disabled until performance of hg is non-abysmal
	# Check whether the branch is behind
    #if hg incoming --quiet --noninteractive
    #   set is_behind true
    #end

	# Check whether the branch has diverged
	if test $is_ahead = true -a $is_behind = true
        set is_diverged true
	end

	set -l full_hg_status
	for i in $PROMPT_HG_PROMPT_ORDER
		set i (string upper $i)
		set hg_status (string upper $hg_status)
		if contains $i in $hg_status
			set -l status_symbol PROMPT_HG_STATUS_$i
			set full_hg_status "$$status_symbol$full_hg_status"
		end
	end
    if $is_diverged
        set full_hg_status "$full_hg_status$PROMPT_HG_STATUS_DIVERGED"
    else if $is_ahead
        set full_hg_status "$full_hg_status$PROMPT_HG_STATUS_AHEAD"
    else if $is_behind
        set full_hg_status "$full_hg_status$PROMPT_HG_STATUS_BEHIND"
    end

    set -l status_color "$PROMPT_HG_STATUS_DIRTY_COLOR"

	if test -n "$full_hg_status"
		__prompt_lib_section \
			$status_color \
			"$PROMPT_HG_STATUS_PREFIX$full_hg_status$PROMPT_HG_STATUS_SUFFIX"
    else
        set status_color "$PROMPT_HG_STATUS_CLEAN_COLOR"
        __prompt_lib_section \
            $status_color \
            " $PROMPT_HG_STATUS_CLEAN_SYMBOL "
	end
end
