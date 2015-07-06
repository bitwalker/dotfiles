function dnvm -d 'DNX Version Manager'
  bash -c 'source /usr/local/bin/dnvm.sh; dnvm "$@"; status=$?; exit $status' nil $argv
end

