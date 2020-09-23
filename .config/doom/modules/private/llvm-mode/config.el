(load! "llvm-mode")
(load! "tablegen-mode")

(require 'llvm-mode)
(require 'tablegen-mode)

(use-package! llvm-mode
  :mode "\\.ll$"
  :init (llvm-mode))

(use-package! tablegen-mode
  :mode "\\.td$"
  :init (llvm-mode))
