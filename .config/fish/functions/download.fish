function download -d "Download a wget-compatible url to the Downloads folder"
  wget $argv[1] -O ~/Downloads/(basename $argv[1])
end