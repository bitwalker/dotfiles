# Disable analytics
set -gx HOMEBREW_NO_ANALYTICS 1

# Set up API token for GitHub, if available
if test -f "$XDG_CONFIG_HOME/brew/API_TOKEN"
    set -l token (cat "$XDG_CONFIG_HOME/brew/API_TOKEN")
    set -gx HOMEBREW_GITHUB_API_TOKEN "$token"
end
