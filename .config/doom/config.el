;;; -*- lexical-binding: t; -*-

;; System

(defvar xdg-data (getenv "XDG_DATA_HOME"))
(defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))

(defvar +bitwalker/doom-config-dir
  (expand-file-name "doom" xdg-config))

(setq user-full-name "Paul Schoenfelder"
      user-mail-address "paulschoenfelder@fastmail.com"

      show-trailing-whitespace t)

;; maximize first frame
(set-frame-parameter nil 'fullscreen 'maximized)

;; UI
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))

(defun +bitwalker/setup-ui-common ()
  "Setup common Emacs UI configuration"
  (progn
    (setq-hook! 'minibuffer-setup-hook show-trailing-whitespace nil)
    (setq doom-font (font-spec :family "Fantasque Sans Mono"
                               :size 14))
    (add-to-list 'custom-theme-load-path (expand-file-name "themes" +bitwalker/doom-config-dir))
    ;(exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-initialize)
    ))

(defun +bitwalker/setup-ui-gui ()
  "Additional UI setup only for graphical windows"
  (progn
    (+bitwalker/setup-ui-common)
    (setq doom-theme 'doom-tomorrow-night)
    (exec-path-from-shell-copy-env "PATH")
    ))

(defun +bitwalker/setup-ui-terminal ()
  "Additional UI setup only for terminal windows"
  (progn
    (+bitwalker/setup-ui-common)
    (setq doom-theme 'doom-tomorrow-night)
    ))

(if (display-graphic-p)
    ;; running in graphical emacs
    (+bitwalker/setup-ui-gui)
    ;; running in terminal emacs
    (+bitwalker/setup-ui-terminal))

;; Magit
(setq magit-repository-directories '(("~/src" . 2))
      magit-save-repository-buffers nil
      
      +magit-hub-features t)

;; Projects
(defvar +bitwalker/project-path
  "~/src/github.com/bitwalker"
  "The location of cloned source repositories for projects")

(defun +bitwalker/add-known-projects ()
  (if (file-directory-p +bitwalker/project-path)
      (cl-loop for project-name in (directory-files +bitwalker/project-path)
               do (projectile-add-known-project (expand-file-name project-name +bitwalker/project-path)))
      (warn! "Project path '%s' has not been created!" (file-relative-name +bitwalker/project-path "~"))))

(after! projectile
  (+bitwalker/add-known-projects))

;; Language-specific
(map! :map elixir-mode-map
      :localleader
      (:prefix "c"
        :desc "Expand macro once (line)" :nv "m" #'alchemist-macroexpand-once-current-line
        :desc "Expand macro once (region)" :nv "M" #'alchemist-macroexpand-once-current-region
        :desc "Compile" :nv "c" #'alchemist-compile
        :desc "Compile current file" :nv "f" #'alchemist-compile-file
        :desc "Compile current buffer" :nv "b" #'alchemist-compile-this-buffer)
      (:prefix "f"
        :desc "Toggle between file/tests" :nv "t" #'alchemist-project-toggle-file-and-tests
        :desc "Create file in project" :nv "c" #'alchemist-project-create-file)
      (:prefix "h"
        :desc "Search hex" :nv "s" #'alchemist-hex-search
        :desc "List dependencies" :nv "l" #'alchemist-hex-all-dependencies)
      (:prefix "i"
        :desc "iex" :nv "i" #'alchemist-iex-run
        :desc "iex -S mix" :nv "p" #'alchemist-iex-project-run
        :desc "Compile buffer in IEx" :nv "b" #'alchemist-iex-compile-this-buffer-and-go
        :desc "Compile region in IEx" :nv "r" #'alchemist-iex-send-region-and-go)
      (:prefix "m"
        :desc "mix" :nv "x" #'alchemist-mix
        :desc "mix compile" :nv "c" #'alchemist-mix-compile
        :desc "mix run" :nv "r" #'alchemist-mix-run)
      (:prefix "p"
        :desc "Find test" :nv "f" #'alchemist-project-find-test)
      (:prefix "t"
        :nv "" nil
        :desc "mix test" :nv "a" #'alchemist-mix-test
        :desc "Toggle report" :nv "d" #'alchemist-test-toggle-test-report-display
        :desc "List tests" :nv "t" #'alchemist-test-mode-list-tests
        :desc "Run test at point" :nv "." #'alchemist-mix-test-at-point
        :desc "Rerun last test" :nv "r" #'alchemist-mix-rerun-last-test
        :desc "Run stale tests" :nv "s" #'alchemist-mix-test-stale
        :desc "Test current buffer" :nv "b" #'alchemist-mix-test-this-buffer
        :desc "Test file" :nv "f" #'alchemist-mix-test-file
        :desc "Run tests for current file" :nv "F" #'alchemist-project-run-tests-for-current-file
        :desc "Jump to previous test" :nv "[" #'alchemist-test-mode-jump-to-previous-test
        :desc "Jump to next test" :nv "]" #'alchemist-test-mode-jump-to-next-test
        :desc "Interrupt test process" :nv "k" #'alchemist-report-interrupt-current-process))

(after! elixir-mode
  (add-to-list 'exec-path (expand-file-name "elixir-ls" xdg-data)))

;; Extend projectile keybindings to provide easy way to kill project buffers
(after! projectile
  (map! :map projectile-mode-map
        (:leader
          (:prefix "p"
            :desc "Kill project buffers" :nv "k" #'projectile-kill-buffers
            :desc "Search project with rg" :nv "s" #'+ivy/project-search
            :desc "Switch to project buffer" :nv "b" #'+ivy/switch-workspace-buffer
            :desc "Switch to project buffer or file" :nv "SPC" #'+ivy/switch-buffer))))


(after! treemacs
  (treemacs-follow-mode t))

(after! lsp
  (setq lsp-file-watch-threshold 750))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"))

(after! rustic-flycheck
  (delete 'rustic-clippy flycheck-checkers))

;; Global keybindings
(map!
  :after evil
  (:leader
    (:prefix "t"
      :desc "Change theme" :nv "t" #'counsel-load-theme)
    (:prefix "TAB"
      :desc "Rename workspace" :nv "r" #'+workspace/rename)))

(use-package! clang-format
    :commands (clang-format-region clang-format-buffer))

(after! cc-mode
    ; Set up formatter keybindings
    (map! :map c-mode-map
          :localleader
          (:prefix ("f" . "format")
            "b" #'clang-format-buffer
            "r" #'clang-format-region))
    (map! :map cpp-mode-map
          :localleader
          (:prefix ("f" . "format")
            "b" #'clang-format-buffer
            "r" #'clang-format-region))
    ; Set up formatter for C/C++ when clang-format is present
    (when (executable-find "clang-format")
      (progn
        ; Handle formatting buffers not backed by a file
        (defun bitwalker/clang-format-args ()
          (let ((bufname (buffer-file-name))
                (filename 
                  (if (not (eq 'nil bufname))
                    bufname
                    (cond ((eq major-mode 'c++-mode) "noname.cpp")
                          ((eq major-mode 'c-mode) "noname.c")
                          (t 'nil))))
                (assumef
                  (if (not (eq 'nil filename))
                    ("--assume-filename=%S" filename)
                     "")))
            '("clang-format"
              "--style=file"
              "--fallback-style=none"
              assumef)))
        (set-formatter! 'clang-format 
                        '(bitwalker/clang-format-args)
                        :modes '(c-mode c++-mode)))))
