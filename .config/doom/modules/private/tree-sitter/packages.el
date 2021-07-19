;; -*- no-byte-compile: t; -*-
;;; private/tree-sitter/packages.el

(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "3a600d769bd5da95bf46bec58893934370c6c04f")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "3a600d769bd5da95bf46bec58893934370c6c04f")
