$pgfiles   = gci -path 'env:programfiles' | % { $_.value }
$pgfiles32 = gci -path 'env:programfiles(x86)' | % { $_.Value }

set-alias help   show-help
set-alias subl   (join-path $HOME 'Dropbox\Applications\SublimeText\subl.exe')
set-alias psql   (join-path $pgfiles 'PostgreSQL\9.3\scripts\runpsql.bat')
set-alias wget   get-file
set-alias rmf    remove-all
set-alias rmrf   remove-all
set-alias less   show-less
set-alias ln     new-link
set-alias tuple  new-tuple
set-alias mkdirp new-path