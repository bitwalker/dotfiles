function __asdf_global_java_home
    pushd $HOME
    if asdf current java >/dev/null
        popd $HOME
        asdf where java
    else
        popd $HOME
        if test -f /usr/libexec/java_home
            /usr/libexec/java_home
        else
            echo ''
        end
    end
end

function __asdf_java_home --on-event fish_preexec
    if asdf current java >/dev/null
        set -gx JAVA_HOME (asdf where java)
    else
        set -gx JAVA_HOME (__asdf_global_java_home)
    end
end
