# extracted from https://github.com/fish-shell/fish-shell/issues/417#issuecomment-13116905
function init --on-event init_osx_manpath

  # set important manpaths here to put at the front of $MANPATH
  set --local user_manpaths

  # populate a local variable with directories from /etc/manpaths
  set --local etc_manpaths
  if test -f /etc/manpaths
      for dir in (cat /etc/manpaths)
          if test -d $dir
              set etc_manpaths $etc_manpaths $dir
          end
      end
  end

  # populate a local variable with content of each file in /etc/manpaths.d/* (filesort order)
  set --local etc_manpathsd
  if test -d /etc/manpaths.d
      for file in /etc/manpaths.d/*
          if test -d (cat $file)
              set etc_manpathsd $etc_manpathsd (cat $file)
          end
      end
  end

  # collect paths (more important ones in front)
  set --local manpath_list $user_manpaths $MANPATH $etc_manpaths $etc_manpathsd

  # remove duplicates from the list
  set --local manpath_sorted
  for i in $manpath_list
      if not contains $i $manpath_sorted
          set manpath_sorted $manpath_sorted $i
      end
  end

  # finally, set the MANPATH variable
  set -x MANPATH $manpath_sorted

end
