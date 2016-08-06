#set history filename ~/.../self/var/cache/gdb/history
python
import os
gdb.execute('set history filename ' + os.environ['XDG_CACHE_HOME'] + '/gdb/history')
end

set history save on
set listsize 40
set height 0
set width 0
set print addr on
set print array on
set print pretty on
set print union on
set print object on
set breakpoint pending on
set auto-load safe-path /
