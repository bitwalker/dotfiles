;;; -*- lexical-binding: t; -*-

;; System

(defvar xdg-data (getenv "XDG_DATA_HOME"))
(defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))

(setq user-full-name "Paul Schoenfelder"
      user-mail-address "paulschoenfelder@gmail.com"

      show-trailing-whitespace t)

;; maximize first frame
(set-frame-parameter nil 'fullscreen 'maximized)

;; UI
(setq-hook! 'minibuffer-setup-hook show-trailing-whitespace nil)

(setq doom-font (font-spec :family "Fantasque Sans Mono"
                           :size 14))
(setq doom-theme 'doom-city-lights)

;; Magit
(setq magit-repository-directories '(("~/src" . 2))
      +magit-hub-features t)

;; Ensure binaries from PATH are found
;; But only on macOS/Linux
(def-package! exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :config (exec-path-from-shell-copy-env "PATH"))

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
      (defun bitwalker/advice-projectile-use-rg ()
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
