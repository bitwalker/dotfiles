;; -*- no-byte-compile: t; -*-
;;; private/tree-sitter/packages.el

(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "258eaa98c95b0afce12bfc1a87e6e9d23b13ba8c")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "25186cdebb7212d1c513e02ac145bf50f356d5db")
