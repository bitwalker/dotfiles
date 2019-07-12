function __prompt_section_golang -d "Display the current go version if you're inside GOPATH"
	__prompt_util_set_default PROMPT_GOLANG_SHOW true
	__prompt_util_set_default PROMPT_GOLANG_PREFIX $PROMPT_DEFAULT_PREFIX
	__prompt_util_set_default PROMPT_GOLANG_SUFFIX $PROMPT_DEFAULT_SUFFIX
	__prompt_util_set_default PROMPT_GOLANG_SYMBOL "🐹 "
	__prompt_util_set_default PROMPT_GOLANG_COLOR cyan

	# Show the current version of Golang
	[ $PROMPT_GOLANG_SHOW = false ]; and return

	# Ensure the go command is available
	type -q go; or return

    # Do nothing if the current directory isn't a Go project
	if not test -f go.mod \
		-o -d Godeps \
		-o -f glide.yaml \
		-o (count *.go) -gt 0 \
		-o -f Gopkg.yml \
		-o -f Gopkg.lock \
		-o ([ (count $GOPATH) -gt 0 ]; and string match $GOPATH $PWD)
		return
	end

	set -l go_version (go version | string split ' ')

	# Go version is either the commit hash and date (devel +5efe9a8f11 Web Jan 9 07:21:16 2019 +0000)
	# at the time of the build or a release tag (go1.11.4)
	# https://github.com/matchai/spacefish/issues/137
	if test (string match 'devel*' $go_version[3])
		set go_version $go_version[3]":"(string sub -s 2 $go_version[4])
	else
		set go_version "v"(string sub -s 3 $go_version[3])
	end

	__prompt_lib_section \
		$PROMPT_GOLANG_COLOR \
		$PROMPT_GOLANG_PREFIX \
		"$PROMPT_GOLANG_SYMBOL""$go_version" \
		$PROMPT_GOLANG_SUFFIX
end

