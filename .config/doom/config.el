;;; -*- lexical-binding: t; -*-

;; System

(defvar xdg-data (getenv "XDG_DATA_HOME"))
(defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))

(defvar +bitwalker/doom-config-dir
  (expand-file-name "doom" xdg-config))

(setq user-full-name "Paul Schoenfelder"
      user-mail-address "paulschoenfelder@fastmail.com"

      show-trailing-whitespace t
      ;; Line numbers are pretty slow, better to not use them by default
      display-line-numbers-type nil
      ;; Require completion to be initiated manually
      company-idle-delay nil
      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so disable by default
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil
      +lsp-prompt-to-install-server nil

      treemacs-width 32)

;; Maximize first frame
(set-frame-parameter nil 'fullscreen 'maximized)

;; Prevents some cases of flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

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
    (setq +treemacs-git-mode 'deferred)
    ))

(defun +bitwalker/setup-ui-gui ()
  "Additional UI setup only for graphical windows"
  (progn
    (+bitwalker/setup-ui-common)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)
    (setq doom-theme 
          'doom-ayu-mirage)
          ;'doom-snazzy)
    (exec-path-from-shell-copy-env "PATH")
    ))

(defun +bitwalker/setup-ui-terminal ()
  "Additional UI setup only for terminal windows"
  (progn
    (+bitwalker/setup-ui-common)
    (setq doom-theme 'doom-ayu-mirage)
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

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

(after! projectile
  (+bitwalker/add-known-projects))

;; Language-specific
(after! elixir-mode
  (add-to-list 'exec-path (expand-file-name "elixir-ls" xdg-data))
  (map! :map elixir-mode-map
      (:localleader
        :desc "Expand macro once (line)" :nv "cm" #'alchemist-macroexpand-once-current-line
        :desc "Expand macro once (region)" :nv "cM" #'alchemist-macroexpand-once-current-region
        :desc "Compile" :nv "cc" #'alchemist-compile
        :desc "Compile current file" :nv "cf" #'alchemist-compile-file
        :desc "Compile current buffer" :nv "cb" #'alchemist-compile-this-buffer
        :desc "Toggle between file/tests" :nv "ft" #'alchemist-project-toggle-file-and-tests
        :desc "Create file in project" :nv "fc" #'alchemist-project-create-file
        :desc "Search hex" :nv "hs" #'alchemist-hex-search
        :desc "List dependencies" :nv "hl" #'alchemist-hex-all-dependencies
        :desc "iex" :nv "ii" #'alchemist-iex-run
        :desc "iex -S mix" :nv "ip" #'alchemist-iex-project-run
        :desc "Compile buffer in IEx" :nv "ib" #'alchemist-iex-compile-this-buffer-and-go
        :desc "Compile region in IEx" :nv "ir" #'alchemist-iex-send-region-and-go
        :desc "mix" :nv "mx" #'alchemist-mix
        :desc "mix compile" :nv "mc" #'alchemist-mix-compile
        :desc "mix run" :nv "mr" #'alchemist-mix-run
        :desc "Find test" :nv "pf" #'alchemist-project-find-test
        :desc "mix test" :nv "ta" #'alchemist-mix-test
        :desc "Toggle report" :nv "td" #'alchemist-test-toggle-test-report-display
        :desc "List tests" :nv "tt" #'alchemist-test-mode-list-tests
        :desc "Run test at point" :nv "t." #'alchemist-mix-test-at-point
        :desc "Rerun last test" :nv "tr" #'alchemist-mix-rerun-last-test
        :desc "Run stale tests" :nv "ts" #'alchemist-mix-test-stale
        :desc "Test current buffer" :nv "tb" #'alchemist-mix-test-this-buffer
        :desc "Test file" :nv "tf" #'alchemist-mix-test-file
        :desc "Run tests for current file" :nv "tF" #'alchemist-project-run-tests-for-current-file
        :desc "Jump to previous test" :nv "t[" #'alchemist-test-mode-jump-to-previous-test
        :desc "Jump to next test" :nv "t]" #'alchemist-test-mode-jump-to-next-test
        :desc "Interrupt test process" :nv "tk" #'alchemist-report-interrupt-current-process)))

;; Extend projectile keybindings to provide easy way to kill project buffers
(after! projectile
  (map! :map projectile-mode-map
        (:leader
          (:prefix "p"
            :desc "Kill project buffers" :nv "k" #'projectile-kill-buffers
            :desc "Search project with rg" :nv "s" #'+ivy/project-search
            :desc "Switch to project buffer" :nv "b" #'+ivy/switch-workspace-buffer
            :desc "Switch to project buffer or file" :nv "SPC" #'+ivy/switch-buffer))))

(after! lsp
  (setq lsp-file-watch-threshold 1000
        lsp-rust-server 'rust-analyzer))

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
    (map! :map (c-mode-map c++-mode-map)
          (:localleader
            :desc "Format buffer" "bf" #'clang-format-buffer))
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
