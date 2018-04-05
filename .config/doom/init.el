;;;  -*- lexical-binding: t; -*-

;; Private modules (found in XDG_CONFIG_HOME/doom/modules/private
(doom! :lang
       erlang

       ; :private
       ; name_of_private_module
       )

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
