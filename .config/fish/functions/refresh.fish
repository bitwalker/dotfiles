function refresh -d "Refresh fish session by replacing current process"
  history --save
  exec fish < /dev/tty
end
