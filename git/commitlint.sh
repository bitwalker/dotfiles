#!/usr/bin/env bash

COMMITLINT_CONFIG="commitlint.config.json"

if [ "$#" -eq 0 ] && [ -z "$COMMIT_MESSAGE_FILE" ]; then
    echo "Expected either an argument or COMMIT_MSG set in the environment!"
    exit 2
elif [ -z "$COMMIT_MESSAGE_FILE" ]; then
    COMMIT_MESSAGE_FILE="$1"
fi

# If jq is not present, warn, but do not prevent the commit
if ! type -p jq >/dev/null; then
    echo "Unable to verify commit message, jq is not installed"
    exit 0
fi

# If a local config isn't present, try the global config
if [ ! -f "${COMMITLINT_CONFIG}" ]; then
    COMMITLINT_CONFIG="${XDG_CONFIG_HOME}/commitlint/config.json"
fi

# If we still don't have a config, skip linting
if [ ! -f "${COMMITLINT_CONFIG}" ]; then
    echo "Skipping commit message linting, could not find commitlint config!"
    exit 0
fi

# Allow disabling the config
enabled="$(jq -r '.enabled' "${COMMITLINT_CONFIG}")"
if [ "$enabled" != "true" ]; then
    exit 0
fi

# Configuration
types="$(jq -r '.types[]' "${COMMITLINT_CONFIG}")"
num_types="$(echo "$types" | wc -l)"

# Start building pattern to match against
regexp="^("

# Allow prefixing of `revert:`
regexp="${regexp}revert:\s+.+)|("

# Add all types to an alternation list
i=0
types_arr=()
for ty in $types; do
    i=$(expr $i + 1)
    types_arr+=("$ty")
    if [ "$i" -lt "$num_types" ]; then
        regexp="${regexp}$ty|"
    else
        regexp="${regexp}$ty)"
    fi
done

# Types can be followed by an optional scope in parantheses
regexp="${regexp}(\(.+\))?"

# The rest of the message can be anything
regexp="${regexp}:\s.+$"

# Perform the lint
msg="$(head -n1 "$COMMIT_MESSAGE_FILE")"
if ! echo "$msg" | grep -E "$regexp" >/dev/null; then
    printf "\n\e[1m\e[31m%s\e[0m\n" "You forgot to format your commit subject line!"
    printf "\e[1m%s:\e[0m\n\n    %s\n\n" "Expected format is" "$regexp"
    printf "\e[1m%s:\e[1m\e[0m\n\n" "Valid Types"
    #printf "\e[1m    %s:\e[1m[0m \e[34m\n%s\033[0m\n\n" "Valid Types" "${types[*]}"
    for ty in ${types[@]}; do
        printf "\e[1m      * %s\e[0m\n" "$ty"
    done
    exit 1
fi
