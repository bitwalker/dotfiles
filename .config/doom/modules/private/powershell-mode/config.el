;;; -*- lexical-binding: t; -*-

(load! "powershell")

(use-package! powershell-mode
              :defer t
              :mode "\\.ps1$"
              :mode "\\.ps1.eex")
