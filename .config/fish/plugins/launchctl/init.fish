# Ensure PATH is exposed to GUI apps
if type -q launchctl
    launchctl setenv PATH (echo $PATH | sed -e 's| /|:/|g' -e 's| ./|:./|g')
end
