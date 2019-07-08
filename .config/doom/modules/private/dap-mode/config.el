;;; dap-mode/config.el -*- lexical-binding: t; -*-

;; The repo to clone when building ElixirLS
(setq elixir-ls-repo "https://github.com/elixir-lsp/elixir-ls")
;; The cache directory to clone ElixirLS into
(setq elixir-ls-cache-path "$HOME/.cache/elixir-ls")
;; The directory to install the ElixirLS release binaries
(setq elixir-ls-bin-path "$HOME/bin")

;; Used to set the current working directory in execvp
(setq override-current-working-directory nil)

;; Helpers for extra tools

(defmacro with-cwd! (dir fun)
  "Execute `fun` within `dir`"
  `(progn
      (setq override-current-working-directory ,dir)
      (let* ((output ,fun))
        (message "output: %s" output)
        (setq override-current-working-directory nil)
        output)))

(defun execvp (&rest argv)
  "Executes a command and returns the result as a string"
  (let* ((args (mapconcat 'shell-quote-argument argv " "))
         (cmd (if override-current-working-directory
                (concat "cd" override-current-working-directory "&&" args)
                args)))
    (progn
        (message "cmd: %s" cmd)
        (message "cwd: %s" override-current-working-directory)
        (shell-command-to-string cmd))))

(defun git (&rest args)
  "Executes git and returns the result as a string"
  (apply 'execvp (cons "git" args)))

(defun mix (&rest args)
  "Executes Mix and returns the result as a string"
  (apply 'execvp (cons "mix" args)))

(defun get-elixir-ls-version ()
  "Fetches the current hash of the ElixirLS installation"
  (let* ((cache-path (expand-file-name elixir-ls-cache-path)))
      (if (file-directory-p cache-path)
          (with-cwd! cache-path
                (git "rev-parse" "--short" "HEAD"))
          nil)))

;; Track changes so we only rebuild if ElixirLS was modified
(setq elixir-ls-should-rebuild nil)
(setq elixir-ls-version nil)

(defun clone-elixir-ls ()
  "Fetches the ElixirLS source, or updates the cached copy"
  (let* ((cache-path (expand-file-name elixir-ls-cache-path)))
      (if (file-directory-p cache-path)
        (with-cwd! cache-path
            (progn
                (git "fetch" "origin" "master")
                (let* ((ver (get-elixir-ls-version)))
                  (setq elixir-ls-should-rebuild (string= ver elixir-ls-version)))))
        (progn
          (git "clone" elixir-ls-repo cache-path)
          (setq elixir-ls-should-rebuild t)))))

(defun install-elixir-ls ()
  "Builds and installs the ElixirLS binaries if not available or if updated"
  (mix "do" "deps.get, compile")
  (mix "elixir_ls.release" "-o" (expand-file-name elixir-ls-bin-path)))

(defun ensure-elixir-ls ()
  "Installs the ElixirLS binaries if not present"
  (execvp "mkdir" "-p" (expand-file-name "$HOME/.cache"))
  (setq elixir-ls-version (get-elixir-ls-version))
  (clone-elixir-ls)
  (when elixir-ls-should-rebuild (install-elixir-ls)))

(after! dap-mode
    ;; Enable DAP
    (dap-mode t)
    (dap-ui-mode t)
    ;; Setup LLDB integration
    (require 'dap-gdb-lldb)
    (dap-gdb-lldb-setup)
    ;; Enable various language modes if flagged with support
    (when (featurep! :lang go +lsp)
        (require 'dap-go))
    (when (featurep! :lang elixir +lsp)
        ;;(ensure-elixir-ls)
        (require 'dap-elixir)))
