;;; private/erlang/init.el -*- lexical-binding: t; -*-

;; Set up erlang-flymake to work with rebar3
(defun rebar3/erlang-flymake-get-include-dirs ()
  (append
     (erlang-flymake-get-include-dirs)
     (file-expand-wildcards (concat (erlang-flymake-get-app-dir) "_build/*/lib"))))

(defun rebar3/erlang-flymake-get-code-path-dirs ()
  (append
     (erlang-flymake-get-code-path-dirs)
     (file-expand-wildcards (concat (erlang-flymake-get-app-dir) "_build/*/lib/*/ebin"))))

(def-package-hook! erlang
  :post-config
  (progn
    (require 'erlang-flymake)

    (setq erlang-flymake-get-code-path-dirs-function 'rebar3/erlang-flymake-get-code-path-dirs)
    (setq erlang-flymake-get-include-dirs-function 'rebar3/erlang-flymake-get-include-dirs)))
