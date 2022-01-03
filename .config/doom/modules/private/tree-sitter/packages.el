;; -*- no-byte-compile: t; -*-
;;; private/tree-sitter/packages.el

(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "48b06796a3b2e76ce004972d929de38146eafaa0")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "e537b90bbca6b4deb62042240ed12461251b3f0c")
