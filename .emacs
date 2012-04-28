;;; Load path exceptions
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/mine")

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;; Evil
(require 'evil)
(evil-mode 1)
(defun evil-passthrough ()
  "Accept a key, switch to emacs state, process that key, and leave emacs state."
    (interactive)
      (let ((next-key (read-event)))
        (progn
          (evil-emacs-state)
            (setq unread-command-events (list next-key))
              (run-with-idle-timer 1 nil (lambda () (evil-exit-emacs-state))))))
(global-set-key (kbd "M-p") 'evil-passthrough)

;;; Set colors
(require 'color-theme)
(require 'color-theme-tomorrow)
  (color-theme-tomorrow-night-eighties)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;;; Use smex
(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;; Powerline
;;;(require 'powerline)

;;; Remap cmd => ctrl
(setq mac-command-modifier 'control)

;;; Alias yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Disable bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;; Prevent startup messages
(setq inhibit-startup-message t)

;;; Hilight matching parens
(show-paren-mode 1)

;;; Force C-n to add newlines at end of buffer
(setq next-line-add-newlines t)

;;; ido mode
(require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;;; Bind M-i to ido-goto-symbol
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;;; Seed random number generator
(random t)

;;; Reduce visual distractions
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;;; Auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(multi-term-scroll-to-bottom-on-output t)
 '(term-default-bg-color "#000000"))

;;; kpm-list
(require 'kpm-list)

;;; Mine
(require 'mine-functions)
(require 'mine-bindings)

;;; Set pager so that CLI apps that use a pager work
(setenv "PAGER" "/bin/cat")

;;; Enable paredit in clojure mode
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

;;; Configure buffer-move
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kdb "<C-S-left")   'buf-move-left)
(global-set-key (kdb "<C-S-right>") 'buf-move-right)
