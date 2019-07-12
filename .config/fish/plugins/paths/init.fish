# Add important paths to PATH (order is lowest-priority first)
for p in /{bin,sbin} /usr/{bin,sbin} /usr/local/{bin,sbin}
    if not contains $p $PATH
        set -p PATH $p
    end
end

# Ensure MANPATH is set and exported
set -gx MANPATH $MANPATH
for p in /usr/share/man /usr/local/share/man
    if not contains $p $MANPATH
        set -p MANPATH $p
    end
end

function __paths_on_change --on-variable PWD
    # Dynamically add project-local Node executables to PATH,
    # and clean them up when no longer present
    if test -d .node_modules/bin
        set -p PATH .node_modules/bin
    else 
        if set -l idx (contains -i .node_modules/bin $PATH)
            set -e PATH[$idx]
        end
    end
end

# We have to call this at least once to capture the startup directory
__paths_on_change
