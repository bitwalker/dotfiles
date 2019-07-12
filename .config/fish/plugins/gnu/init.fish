# When coreutils are installed, setting these paths will allow using the binaries
# without their g_ prefix, and will allow the man files to be loaded.

if test -d /usr/local/opt/coreutils/libexec/gnubin
    set -gx GNU_PATH /usr/local/opt/coreutils/libexec/gnubin
    set PATH $GNU_PATH $PATH
end

if test -d /usr/local/opt/coreutils/libexec/gnuman
    set -gx GNU_MANPATH /usr/local/opt/coreutils/libexec/gnuman
end

for p in /usr/local/opt/coreutils/share/man /usr/local/opt/coreutils/libexec/gnuman
    if test -d $p
        and not contains $p $MANPATH
        set MANPATH $p $MANPATH
    end
end
