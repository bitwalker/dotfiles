function __prompt_util_hg_find_root -d "Find the Mercurial root"
  set -l dir (pwd)
  set -e HG_ROOT

  while test $dir != "/"
    if test -f "$dir/.hg/dirstate"
      set -g HG_ROOT "$dir/.hg"
      return 0
    end

    set dir (dirname $dir)
  end

  return 1
end
