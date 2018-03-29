;;; private/erlang/config.el -*- lexical-binding: t; -*-

;; Completion via Ivy
(def-package! erlang-ivy
  :when (featurep! :completion ivy)
  :after (:all erlang ivy-erlang-complete)
  :config
  (add-hook 'erlang-mode-hook #'ivy-erlang-complete-init)
  (add-hook 'after-save-hook #'ivy-erlang-complete-reparse))


;; Completion via Company
(def-package! erlang-company
  :when (featurep! :completion company)
  :after (:all erlang company-erlang)
  :config
  (add-hook 'erlang-mode-hook #'company-erlang-init))

;; Extend base erlang-mode
(after! erlang
  (progn
    ;; rebar files
    (add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))
    (add-to-list 'auto-mode-alist '("rebar.config.script" . erlang-mode))
    ;; erlang configs
    (add-to-list 'auto-mode-alist '("app.config" . erlang-mode))
    (add-to-list 'auto-mode-alist '("sys.config" . erlang-mode))
    ;; customizations
    (add-to-list 'auto-mode-alist '("\\.erlang$" . erlang-mode))))


