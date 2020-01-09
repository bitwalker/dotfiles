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
    ))

(defun +bitwalker/setup-ui-gui ()
  "Additional UI setup only for graphical windows"
  (progn
    (+bitwalker/setup-ui-common)
    (setq doom-theme 'doom-city-lights)
    (exec-path-from-shell-copy-env "PATH")
    ))

(defun +bitwalker/setup-ui-terminal ()
  "Additional UI setup only for terminal windows"
  (progn
    (+bitwalker/setup-ui-common)
    (setq doom-theme 'bitwalker-laserwave)
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

(defun +bitwalker/alchemist-iex-send-buffer ()
    "Sends the current buffer to the IEx process and evaluates it"
    (interactive)
    (alchemist-iex--send-command (alchemist-iex-process) (buffer-string)))

;; Seems to kick in during *any* evil mode, which obviously isn't ideal with SPC as leader
;;(setq alchemist-key-command-prefix (kbd doom-localleader-key))
(after! alchemist
  (map! :map elixir-mode-map
        :nv "SPC m" alchemist-mode-keymap
        (:localleader
          (:prefix "i" :nv "B" #'+bitwalker/alchemist-iex-send-buffer))))

;; Extend projectile keybindings to provide easy way to kill project buffers
(after! projectile
  (when (executable-find "rg")
    (progn
      (defconst bitwalker/rg-arguments
        `("--line-number"  ; line numbers
          "--smart-case"
          "--follow"       ; follow symlinks
          "--mmap")        ; apply memory map optimization when possible
        "Default rg arguments used in `projectile' project file listings.")
      (defun bitwalker/advice-projectile-use-rg (vcs)
        "Always use `rg' for getting a list of all files in the project."
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                            bitwalker/rg-arguments
                            '("--null" ; output null separated results
                              "--files")) ; get file names matching the empty regex (all files)
                   " "))
      (advice-add 'projectile-get-ext-command :override #'bitwalker/advice-projectile-use-rg)))
  (map! :map projectile-mode-map
        (:leader
          (:prefix "p"
            :desc "Kill project buffers" :nv "k" #'projectile-kill-buffers
            :desc "Search project with rg" :nv "s" #'counsel-projectile-rg
            :desc "Switch to project buffer" :nv "b" #'counsel-projectile-switch-to-buffer
            :desc "Switch to project buffer or file" :nv "SPC" #'counsel-projectile))))


(after! treemacs
  (treemacs-follow-mode t))

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
