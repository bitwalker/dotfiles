function fish_prompt
	set_color yellow
   printf '%s' (whoami)
   set_color normal
   printf ' at '

   set_color magenta
   printf '%s' (hostname|cut -d . -f 1)
   set_color normal
   printf ' in '

   set_color $fish_color_cwd
   printf '%s' (prompt_pwd)
   set_color normal

   set -l cyan (set_color -o cyan)
   set -l yellow (set_color -o yellow)
   set -l red (set_color -o red)
   set -l blue (set_color -o blue)
   set -l normal (set_color normal)

   function _git_branch_name
      echo (git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||') 
   end 
   function _is_git_dirty
      echo (git status -s --ignore-submodules=dirty ^/dev/null) 
   end 
   if [ (_git_branch_name) ]
      set -l git_branch $red(_git_branch_name)
      set git_info "$blue git:($git_branch$blue)" 
   end
   if [ (_is_git_dirty) ]
      set -l dirty "$yellow ✗"
      set git_info "$git_info$dirty"
   end

   printf '%s' $git_info

   # Line 2
   echo
   if test $VIRTUAL_ENV
       printf "(%s) " (set_color blue)(basename $VIRTUAL_ENV)(set_color normal)
   end
   printf '↪ '
   set_color normal
end
