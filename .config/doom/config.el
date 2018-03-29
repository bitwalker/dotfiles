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

(setq alchemist-key-command-prefix (kbd doom-localleader-key))
