if type -q direnv
    function __direnv_export_eval --on-event fish_preexec
        direnv export fish | source;
    end
end
