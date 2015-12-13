function fetch -a url -d "Download all files in an open directory to the current directory"
    command wget -m --no-parent \
                 --restrict-file-names=nocontrol \
                 --user-agent='Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:36.0) Gecko/20100101 Firefox/36.0 [direct download]' \
                 --referer='http://www.google.com' \
                 '$argv[1]'
end
