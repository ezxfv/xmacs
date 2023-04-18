(when (modulep! :lang python +pylance)
  (require 'lsp)

  ;; download vsix from `https://www.vsixhub.com/vsix/41816/'
  ;; install: code install --install-extension vscode-pylance-2020.10.2_vsixhub.com.vsix --force
  (setq pylancejs (concat (getenv "HOME") "/.vscode/extensions/ms-python.vscode-pylance-2020.10.2/dist/server.bundle.js"))
  (unless (file-exists-p pylancejs)
    (setq pylancejs (concat (getenv "HOME") "/.vscode-server/extensions/ms-python.vscode-pylance-2020.10.2/dist/server.bundle.js"))
    )
  (setq lsp-pyright-server-cmd (list "node" pylancejs "--stdio"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () lsp-pyright-server-cmd)
                     (lambda ()
                       (and (cl-second lsp-pyright-server-cmd)
                            (file-exists-p (cl-second lsp-pyright-server-cmd)))))
    :major-modes '(python-mode)
    :server-id 'pyright
    :priority 11
    :multi-root t
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                                          (lsp--set-configuration (lsp-configuration-section "python"))))
    :notification-handlers (lsp-ht ("pyright/beginProgress" 'ignore)
                                   ("pyright/reportProgress" 'ignore)
                                   ("pyright/endProgress" 'ignore))))
  )


;; These MODE-local-vars-hook hooks are a Doom thing. They're executed after
;; MODE-hook, on hack-local-variables-hook. Although `lsp!` is attached to
;; python-mode-local-vars-hook, it should occur earlier than my-flycheck-setup
;; this way:
(add-hook 'python-mode-local-vars-hook (lambda ()
                                         (semantic-mode 1)
                                         (when (file-directory-p "~/.pyenv/versions/inf")
                                           (setq lsp-pyright-venv-path "~/.pyenv/versions/inf")
                                           (pyenv-mode-set "inf")
                                           )
                                         (when (getenv "CONDA_PREFIX_1")
                                           (setq conda-anaconda-home (getenv "CONDA_PREFIX_1"))
                                           (setq conda-env-autoactivate-mode 1)
                                           (conda-env-activate "gitops")
                                           )
                                         (setq python-shell-interpreter "ipython"
                                               python-shell-interpreter-args "-i"
                                               doom-modeline-env-python-executable "python3"
                                               lsp-pyls-plugins-autopep8-enabled t
                                               lsp-pyls-plugins-pycodestyle-enabled t
                                               lsp-pyls-plugins-flake8-max-line-length 120
                                               lsp-pyright-auto-import-completions t)
                                         (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
                                         (when (modulep! :tools syntax)
                                           (setq
                                            flycheck-python-pycompile-executable "python3"
                                            flycheck-python-pylint-executable "python3"
                                            flycheck-python-flake8-executable "python3"
                                            flycheck-python-pycompile-executable "python3")
                                           (flycheck-mode 1)
                                           (flycheck-add-next-checker 'python-pyright '(warning . python-pylint))
                                           (flycheck-select-checker 'python-pyright))))
