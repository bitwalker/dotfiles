# When available, make use of ccache
if type -q ccache
    set -gx USE_CCACHE 1
    set -q CCACHE_ROOT; or set -l CCACHE_ROOT (type -p ccache)
    set -p PATH $CCACHE_ROOT
end
