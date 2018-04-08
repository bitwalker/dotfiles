# Add `~/bin` to the `$PATH`
export XDG_CONFIG_HOME="$HOME/.config"
export GOPATH="$HOME"
export JAVA_HOME=$(/usr/libexec/java_home)
export NPM_BIN=./node_modules/.bin
export PATH="$NPM_BIN:$GOPATH/bin:$HOME/bin:/usr/local/bin:$PATH";

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{path,bash_prompt,exports,aliases,functions,extra}; do
  [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
  shopt -s "$option" 2> /dev/null;
done;

# Add tab completion for many Bash commands
# Add tab completion for many Bash commands
if which brew &> /dev/null && [ -f "$(brew --prefix)/share/bash-completion/bash_completion" ]; then
	source "$(brew --prefix)/share/bash-completion/bash_completion";
elif [ -f /etc/bash_completion ]; then
	source /etc/bash_completion;
fi;

# Enable tab completion for `g` by marking it as an alias for `git`
if type _git &> /dev/null && [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
  complete -o default -o nospace -F _git g;
fi;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# Add tab completion for `defaults read|write NSGlobalDomain`
# You could just use `-g` instead, but I like being explicit
complete -W "NSGlobalDomain" defaults;

# Add `killall` tab completion for common apps
complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal Twitter" killall;

if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Frontend to controlling prompt
prompt() {

    # If no arguments, print the prompt strings as they are
    if ! (($#)) ; then
        declare -p PS1 PS2 PS3 PS4
        return
    fi

    # What's done next depends on the first argument to the function
    case $1 in

        # Turn complex, colored PS1 and debugging PS4 prompts on
        on)
            # Declare the PROMPT_RETURN variable as an integer
            declare -i PROMPT_RETURN

            # Set up pre-prompt command
            PROMPT_COMMAND='PROMPT_RETURN=$? ; history -a'

            # Set up prompt, including optional PROMPT_PREFIX and PROMPT_SUFFIX
            # variables
            PS1='\w '
            PS1=$PS1'$(prompt vcs)'
            PS1=$PS1'$(prompt job)'
            PS1=$PS1'$(prompt ret)'
            PS1='${PROMPT_PREFIX}'$PS1
            PS1=$PS1'${PROMPT_SUFFIX}'
            PS1=$PS1' \$'

            # If Bash 4.0 is available, trim very long paths in prompt
            if ((BASH_VERSINFO[0] >= 4)) ; then
                PROMPT_DIRTRIM=4
            fi

            # Count available colors
            local -i colors
            colors=$( {
                tput Co || tput colors
            } 2>/dev/null )

            # Prepare reset code
            local reset
            reset=$( {
                tput me || tput sgr0
            } 2>/dev/null )

            # Decide prompt color formatting based on color availability
            local format
            case $colors in

                # Check if we have non-bold bright cyan available
                256)
                    format=$( {
                        : "${PROMPT_COLOR:=6}"
                        tput AF "$PROMPT_COLOR" ||
                        tput setaf "$PROMPT_COLOR" ||
                        tput AF "$PROMPT_COLOR" 0 0  ||
                        tput setaf "$PROMPT_COLOR" 0 0
                    } 2>/dev/null )
                    ;;

                # If we have only eight colors, use bold cyan
                8)
                    format=$( {
                        : "${PROMPT_COLOR:=6}"
                        tput AF "$PROMPT_COLOR" ||
                        tput setaf "$PROMPT_COLOR"
                        tput md || tput bold
                    } 2>/dev/null )
                    ;;

                # For all other terminals, we assume non-color (!), and we just
                # use bold
                *)
                    format=$( {
                        tput md || tput bold
                    } 2>/dev/null )
                    ;;
            esac

            # String it all together
            PS1='\['"$format"'\]'"$PS1"'\['"$reset"'\] '
            PS2='> '
            PS3='? '
            PS4='+<$?> ${BASH_SOURCE:-$BASH}:$FUNCNAME:$LINENO:'
            ;;

        # Revert to simple inexpensive prompts
        off)
            unset -v PROMPT_COMMAND PROMPT_DIRTRIM PROMPT_RETURN
            PS1='\$ '
            PS2='> '
            PS3='? '
            PS4='+ '
            ;;

        git)
            # Bail if we have no git(1)
            if ! hash git 2>/dev/null ; then
                return 1
            fi

            # Attempt to determine git branch, bail if we can't
            local branch
            branch=$( {
                git symbolic-ref --quiet HEAD ||
                git rev-parse --short HEAD
            } 2>/dev/null )
            if [[ ! -n $branch ]] ; then
                return 1
            fi
            branch=${branch##*/}

            # Safely read status with -z --porcelain
            local line
            local -i ready modified untracked
            while IFS= read -rd '' line ; do
                if [[ $line == [MADRCT]* ]] ; then
                    ready=1
                fi
                if [[ $line == ?[MADRCT]* ]] ; then
                    modified=1
                fi
                if [[ $line == '??'* ]] ; then
                    untracked=1
                fi
            done < <(git status -z --porcelain 2>/dev/null)

            # Build state array from status output flags
            local -a state
            if ((ready)) ; then
                state[${#state[@]}]='+'
            fi
            if ((modified)) ; then
                state[${#state[@]}]='!'
            fi
            if ((untracked)) ; then
                state[${#state[@]}]='?'
            fi

            # Add another indicator if we have stashed changes
            if git rev-parse --verify refs/stash >/dev/null 2>&1 ; then
                state[${#state[@]}]='^'
            fi

            # Print the status in brackets with a git: prefix
            (IFS= ; printf '(git:%s%s)' "${branch:-unknown}" "${state[*]}")
            ;;

        # Mercurial prompt function
        hg)
            # Bail if we have no hg(1)
            if ! hash hg 2>/dev/null ; then
                return 1
            fi

            # Exit if not inside a Mercurial tree
            local branch
            if ! branch=$(hg branch 2>/dev/null) ; then
                return 1
            fi

            # Safely read status with -0
            local line
            local -i modified untracked
            while IFS= read -rd '' line ; do
                if [[ $line == '?'* ]] ; then
                    untracked=1
                else
                    modified=1
                fi
            done < <(hg status -0 2>/dev/null)

            # Build state array from status output flags
            local -a state
            if ((modified)) ; then
                state[${#state[@]}]='!'
            fi
            if ((untracked)) ; then
                state[${#state[@]}]='?'
            fi

            # Print the status in brackets with an hg: prefix
            (IFS= ; printf '(hg:%s%s)' "${branch:-unknown}" "${state[*]}")
            ;;

        # Subversion prompt function
        svn)
            # Bail if we have no svn(1)
            if ! hash svn 2>/dev/null ; then
                return 1
            fi

            # Determine the repository URL and root directory
            local key value url root
            while IFS=: read -r key value ; do
                case $key in
                    'URL')
                        url=${value## }
                        ;;
                    'Repository Root')
                        root=${value## }
                        ;;
                esac
            done < <(svn info 2>/dev/null)

            # Exit if we couldn't get either
            if [[ ! -n $url || ! -n $root ]] ; then
                return 1
            fi

            # Remove the root from the URL to get what's hopefully the branch
            # name, removing leading slashes and the 'branches' prefix, and any
            # trailing content after a slash
            local branch
            branch=${url/"$root"}
            branch=${branch#/}
            branch=${branch#branches/}
            branch=${branch%%/*}

            # Parse the output of svn status to determine working copy state
            local symbol
            local -i modified untracked
            while read -r symbol _ ; do
                if [[ $symbol == *'?'* ]] ; then
                    untracked=1
                else
                    modified=1
                fi
            done < <(svn status 2>/dev/null)

            # Add appropriate state flags
            local -a state
            if ((modified)) ; then
                state[${#state[@]}]='!'
            fi
            if ((untracked)) ; then
                state[${#state[@]}]='?'
            fi

            # Print the state in brackets with an svn: prefix
            (IFS= ; printf '(svn:%s%s)' "${branch:-unknown}" "${state[*]}")
            ;;

        # VCS wrapper prompt function; print the first relevant prompt, if any
        vcs)
            local vcs
            for vcs in "${PROMPT_VCS[@]:-git}" ; do
                if prompt "$vcs" ; then
                    return
                fi
            done
            ;;

        # Show return status of previous command in angle brackets, if not zero
        ret)
            if ((PROMPT_RETURN > 0)) ; then
                printf '<%u>' "$PROMPT_RETURN"
            fi
            ;;

        # Show the count of background jobs in curly brackets, if not zero
        job)
            local -i jobc
            while read ; do
                ((jobc++))
            done < <(jobs -p)
            if ((jobc > 0)) ; then
                printf '{%u}' "$jobc"
            fi
            ;;

        # Print error
        *)
            printf '%s: Unknown command %s\n' "$FUNCNAME" "$1" >&2
            return 2
            ;;

    esac
}

# Start with full-fledged prompt
prompt on

# Init package managers
source ~/.asdf/asdf.sh
