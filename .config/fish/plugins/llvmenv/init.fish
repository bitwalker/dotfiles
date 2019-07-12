# Export LLVM_SYS_XX_PREFIX if llvmenv is available and configured
if type -q llvmenv
    if test -d "$XDG_CONFIG_HOME/llvmenv"
        set -l llvm_version (llvmenv current | tr -d '[:alpha:]')
        set -l llvm_prefix_var LLVM_SYS_{$llvm_version}_PREFIX
        set -l llvm_prefix (llvmenv prefix)
        set -l llvm_prefix_var_decl "set -gx $llvm_prefix_var $llvm_prefix"
        eval $llvm_prefix_var_decl
    end
end
