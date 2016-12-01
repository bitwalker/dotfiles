function fish_right_prompt
	set -l cyan (set_color green)
  set -l normal (set_color normal)
  printf (date "+$cyan%H:%M:%S$normal")
end
