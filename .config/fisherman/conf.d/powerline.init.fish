function init -a path --on-event init_powerline
  if available powerline-daemon
    powerline-daemon -q
  end

  set -q POWERLINE_PACKAGE_DIR; or set -gx POWERLINE_PACKAGE_DIR (pip show powerline-status 2>/dev/null | grep Location | awk '{ print $2 }')
  set fish_function_path $fish_function_path "$POWERLINE_PACKAGE_DIR/powerline/bindings/fish"

  if available powerline-setup
    powerline-setup
  else
    echo "Please install powerline"
  end
end
