function __prompt_section_kubecontext -d "Display the kubernetes context"
	__prompt_util_set_default PROMPT_KUBECONTEXT_SHOW true
	__prompt_util_set_default PROMPT_KUBECONTEXT_NAMESPACE_SHOW true
	__prompt_util_set_default PROMPT_KUBECONTEXT_PREFIX "at "
	__prompt_util_set_default PROMPT_KUBECONTEXT_SUFFIX $PROMPT_DEFAULT_SUFFIX
	# Additional space is added because ☸️ is wider than other symbols
	# See: https://github.com/denysdovhan/spaceship-prompt/pull/432
    #__prompt_util_set_default PROMPT_KUBECONTEXT_SYMBOL "☸️  "
    __prompt_util_set_default PROMPT_KUBECONTEXT_SYMBOL ⎈
	__prompt_util_set_default PROMPT_KUBECONTEXT_COLOR cyan


	# Show current kubecontext
	[ $PROMPT_KUBECONTEXT_SHOW = false ]; and return
	# Ensure the kubectl command is available
	type -q kubectl; or return

	set -l kube_context (kubectl config current-context 2>/dev/null)
	[ -z $kube_context ]; and return

	if test "$PROMPT_KUBECONTEXT_NAMESPACE_SHOW" = "true" -a "$kube_context" != "default"
		set kube_namespace (kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null)
        if test $kube_context != "default"
            set kube_context "$kube_context ($kube_namespace)"
        end
	end

	__prompt_lib_section \
		$PROMPT_KUBECONTEXT_COLOR \
		$PROMPT_KUBECONTEXT_PREFIX \
		"$PROMPT_KUBECONTEXT_SYMBOL""$kube_context" \
		$PROMPT_KUBECONTEXT_SUFFIX
end
