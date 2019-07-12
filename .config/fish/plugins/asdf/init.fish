# If installed, initialize ASDF package manager
# It does so by registering an init handler that is invoked when the asdf plugin is fully loaded
# This hanlder then ensures ASDF is sourced and completions registered

function __asdf_source_and_complete -a asdf_dir add_complete
  source "$asdf_dir/asdf.fish"
  if test "$add_complete" = "true"
    set fish_complete_path $fish_complete_path[1] "$asdf_dir/completions" $fish_complete_path[2..-1]
  end
end

function init -a path --on-event init_asdf
  set -q ASDF_DIR;
    and test -f $ASDF_DIR/asdf.fish;
    and __asdf_source_and_complete $ASDF_DIR true;
    and return
  test -f $HOME/.asdf/asdf.fish;
    and __asdf_source_and_complete $HOME/.asdf true;
    and return
  test -f /usr/local/opt/asdf/asdf.fish;
    and __asdf_source_and_complete /usr/local/opt/asdf false;
    and return
  return
end
