# SYNOPSIS
#   require [name]
#
# OVERVIEW
#   Require a plugin:
#     - Autoload its functions and completions.
#     - Source its initialization file.
#     - Emit its initialization event.
#
#   If the required plugin has already been loaded, does nothing.
function require -a name -d "Require a plugin"
    set -l plugin_path "$XDG_CONFIG_HOME/fish/plugins/$name"

    # Skip if plugin has already been loaded.
    contains -- "$plugin_path" "$fish_function_path"; and return 0

    # Autoload the functions/completions for the plugin
    autoload "$plugin_path/functions" "$plugin_path/completions"

    # Locate the plugin init script, if one exists
    set -l plugin_init
    # It is allowed for both <plugin_dir>/init.fish and <plugin_dir>/<plugin>.fish to be used
    if test -f "$plugin_path/init.fish"
        set plugin_init "$plugin_path/init.fish"
    else if test -f "$plugin_path/$name.fish"
        set plugin_init "$plugin_path/$name.fish"
    else
        # If no init script is defined, erase the plugin_init var
        set -e plugin_init
    end

    # If the init script was defined, source it and fire the init event
    # If we failed to source the plugin, return an error
    # If there was no init script, just fire the init event
    if set -q plugin_init
        if source "$plugin_init"
            emit init_$name "$plugin_path"
        else
            return $status
        end
    else
        emit init_$name "$plugin_path"
    end

    # Source config files for the plugin, if they exist
    for conf in "$plugin_path"/conf.d/*.fish
        if not source "$conf"
            return $status
        end
    end

    return 0
end

