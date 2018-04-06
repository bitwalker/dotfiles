;; Set font
(setq doom-font (font-spec :family "Fantasque Sans Mono"
                           :size 14))

;; Set theme
(setq doom-theme 'doom-city-lights)

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
  (map! :map projectile-mode-map
        (:leader
          (:prefix "p"
            :desc "Kill project buffers" :nv "k" #'projectile-kill-buffers
            :desc "Search project with rg" :nv "s" #'counsel-projectile-rg
            :desc "Switch to project buffer" :nv "b" #'counsel-projectile-switch-to-buffer
            :desc "Switch to project buffer or file" :nv "SPC" #'counsel-projectile))))

;; Neotree config

;; When opening neotree, jump to current file if possible
(setq neo-smart-open t)

(after! neotree
  ;; When switching to a file in the current project, expand the directory
  ;; tree to the new file buffer, i.e. neotree follows the current buffer
  (add-hook! 'find-file-hook
    (if (and (buffer-file-name) (neo-global--window-exists-p))
        ;; And only if the file is a child of the current neotree root
        (if (neo-global--file-in-root-p (buffer-file-name))
            ;; We need to trigger neotree-find then switch back to the buffer we just opened
            (save-current-buffer (neotree-find))))))

;; Global keybindings
(map!
  :after evil
  (:leader
    (:prefix "t"
      :desc "Change theme" :nv "t" #'counsel-load-theme)
    (:prefix "TAB"
      :desc "Rename workspace" :nv "r" #'+workspace/rename)))
