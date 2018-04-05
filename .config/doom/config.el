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

;; Seems to kick in during *any* evil mode, which obviously isn't ideal with SPC as leader
;;(setq alchemist-key-command-prefix (kbd doom-localleader-key))
(after! alchemist
  (map! :map elixir-mode-map
        :nv "SPC m" alchemist-mode-keymap))

;; Extend projectile keybindings to provide easy way to kill project buffers
(after! projectile
  (map! :map projectile-mode-map
        (:leader
          (:prefix "p"
            :desc "Kill project buffers" :nv "k" #'projectile-kill-buffers
            :desc "Search project with rg" :nv "s" #'counsel-projectile-rg
            :desc "Switch to project buffer" :nv "b" #'counsel-projectile-switch-to-buffer
            :desc "Switch to project buffer or file" :nv "SPC" #'counsel-projectile))))

;; Provide keybind to easily change theme
(map!
  :after evil
  (:leader
    (:prefix "t"
      :desc "Change theme" :nv "t" #'counsel-load-theme)
    (:prefix "TAB"
      :desc "Rename workspace" :nv "r" #'+workspace/rename)))
