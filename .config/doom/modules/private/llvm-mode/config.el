(load! "llvm-mode")
(load! "tablegen-mode")

(require 'llvm-mode)
(require 'tablegen-mode)

(def-package! llvm-mode
  :mode "\\.ll$"
  :init (llvm-mode))

(def-package! tablegen-mode
  :mode "\\.td$"
  :init (llvm-mode))
