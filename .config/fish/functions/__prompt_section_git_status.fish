function __prompt_section_git_status -d "Display the current git status"
	__prompt_util_set_default PROMPT_GIT_STATUS_SHOW true
    __prompt_util_set_default PROMPT_GIT_STATUS_MAX_CHANGES 100
	__prompt_util_set_default PROMPT_GIT_STATUS_PREFIX " ["
	__prompt_util_set_default PROMPT_GIT_STATUS_SUFFIX ]
    __prompt_util_set_default PROMPT_GIT_STATUS_CLEAN_SYMBOL ✓
    __prompt_util_set_default PROMPT_GIT_STATUS_CLEAN_COLOR green
	__prompt_util_set_default PROMPT_GIT_STATUS_DIRTY_COLOR red
	__prompt_util_set_default PROMPT_GIT_STATUS_UNTRACKED \?
    __prompt_util_set_default PROMPT_GIT_STATUS_UNSTAGED ⌁
	__prompt_util_set_default PROMPT_GIT_STATUS_ADDED +
	__prompt_util_set_default PROMPT_GIT_STATUS_MODIFIED !
	__prompt_util_set_default PROMPT_GIT_STATUS_RENAMED »
	__prompt_util_set_default PROMPT_GIT_STATUS_DELETED ✘
	__prompt_util_set_default PROMPT_GIT_STATUS_STASHED \$
	__prompt_util_set_default PROMPT_GIT_STATUS_UNMERGED =
	__prompt_util_set_default PROMPT_GIT_PROMPT_ORDER untracked unstaged added modified renamed deleted stashed unmerged

	[ $PROMPT_GIT_STATUS_SHOW = false ]; and return

	set -l git_status
	set -l full_git_status
    set -l status_color
    set -l is_diverged false
    set -l untracked false
    set -l added false
    set -l modified false
    set -l renamed false
    set -l deleted false
    set -l copied false
    set -l unmerged false
    set -l unstaged false

	# Check for stashes
	if git rev-parse --verify refs/stash >/dev/null 2>&1
		set git_status stashed $git_status
	end

    # Get ahead/behind info
    set -l status_ahead (__prompt_util_git_ahead)

    set -l is_dirty 0
    set -l index (command git --no-optional-locks diff --no-color --name-status --diff-filter=ADMRTUX --ignore-submodules=dirty --cached --exit-code 2>/dev/null)
    set is_dirty $status
	set -l tracked (command git --no-optional-locks diff --no-color --name-status --diff-filter=ADMRTUX --ignore-submodules=dirty --exit-code 2>/dev/null)
    if test $status -ne 0 -o $is_dirty -ne 0
        set is_dirty 1
    end
    
    if test $is_dirty -ne 0
        # There are changes to the index/tracked files
        set -l changes $index $tracked
        for change_type in (string sub --start 1 --length 1 $changes | sort -u)
            switch $change_type
                case 'X'
                    if not $untracked
                        set untracked true
                        set git_status untracked $git_status
                    end
                case 'D'
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
                case 'R'
                    if not $renamed
                        set renamed true
                        set git_status renamed $git_status
                    end
                case 'C'
                    if not $copied
                        set copied true
                        set git_status copied $git_status
                    end
                case 'U'
                    if test $unmerged = false
                        set unmerged true
                        set git_status unmerged $git_status
                    end
                case '*'
                    continue
            end
        end

        set git_status (string upper $git_status)

        for i in $PROMPT_GIT_PROMPT_ORDER
            set i (string upper $i)
            if contains $i in $git_status
                set -l status_symbol PROMPT_GIT_STATUS_$i
                set full_git_status "$$status_symbol$full_git_status"
            end
        end

        if test -n "$status_ahead"
            set full_git_status "$full_git_status $status_ahead"
        end

        set status_color "$PROMPT_GIT_STATUS_DIRTY_COLOR"
        set full_git_status "$PROMPT_GIT_STATUS_PREFIX$full_git_status$PROMPT_GIT_STATUS_SUFFIX"
    else
        # There aren't changes to tracked files, but what about untracked files
        if test -n (echo (command git ls-files --others --exclude-standard --directory --no-empty-directory))
            # Untracked files
            set git_status untracked $git_status
        end

        for i in $PROMPT_GIT_PROMPT_ORDER
            set i (string upper $i)
            set git_status (string upper $git_status)
            if contains $i in $git_status
                set -l status_symbol PROMPT_GIT_STATUS_$i
                set full_git_status "$$status_symbol$full_git_status"
            end
        end

        if not test -n "$full_git_status" -o -n "$status_ahead"
            # Totally clean
            set status_color "$PROMPT_GIT_STATUS_CLEAN_COLOR"
            set full_git_status " $PROMPT_GIT_STATUS_CLEAN_SYMBOL "
        else if not test -n "$full_git_status"
            # Diverged, but clean
            set status_color "$PROMPT_GIT_STATUS_CLEAN_COLOR"
            set full_git_status " $PROMPT_GIT_STATUS_CLEAN_SYMBOL ($status_ahead) "
        else if contains untracked in $git_status
            # Dirty
            set status_color "$PROMPT_GIT_STATUS_DIRTY_COLOR"
            set full_git_status "$PROMPT_GIT_STATUS_PREFIX$full_git_status$PROMPT_GIT_STATUS_SUFFIX"
        else
            # Stashed changes, but clean
            set status_color "$PROMPT_GIT_STATUS_CLEAN_COLOR"
            set full_git_status " $PROMPT_GIT_STATUS_CLEAN_SYMBOL ($full_git_status)"
        end
    end

    __prompt_lib_section \
        $status_color \
        $full_git_status
end
