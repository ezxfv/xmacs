;;; modules/x/lang/+python.el -*- lexical-binding: t; -*-

;; Ruff Language Server 配置
(when (modulep! :lang python +ruff)
  (require 'lsp-mode)
  
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff" "server" "--preview"))
    :activation-fn (lsp-activate-on "python")
    :server-id 'ruff-lsp
    :priority 10
    :add-on? nil
    :multi-root t
    :initialization-options (lambda ()
                              (list :settings
                                    (list :lineLength 88
                                          :lint (list :enable t)
                                          :format (list :enable t)))))))

;; Ruff Flycheck Checker 定义（仅在不使用 ruff LSP 时使用）
(when (and (modulep! :tools syntax)
           (not (modulep! :lang python +ruff)))
  (require 'flycheck)
  
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" "check" "--output-format" "text" "--stdin-filename" source-original source-inplace)
    :standard-input t
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (id (one-or-more (not (any ":")))) ": " (message) line-end))
    :modes python-mode)
  
  (add-to-list 'flycheck-checkers 'python-ruff))

;; Python 环境和工具配置
(add-hook 'python-mode-local-vars-hook (lambda ()
                                         (semantic-mode 1)
                                         ;; Conda 环境配置
                                         (when (getenv "CONDA_PREFIX_1")
                                           (setq conda-anaconda-home (getenv "CONDA_PREFIX_1"))
                                           (setq conda-env-autoactivate-mode 1))
                                         
                                         ;; Python 解释器配置
                                         (setq python-shell-interpreter "ipython"
                                               python-shell-interpreter-args "-i"
                                               doom-modeline-env-python-executable "python3")
                                         
                                         ;; 添加Python3到补全解释器列表
                                         (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
                                         
                                         ;; 根据配置选择合适的检查工具
                                         (cond
                                          ;; 使用 Ruff LSP 时，LSP 会处理检查和格式化
                                          ((modulep! :lang python +ruff)
                                           (when (modulep! :tools lsp)
                                             (lsp-deferred)))
                                          
                                          ;; 使用传统 flycheck + ruff
                                          ((modulep! :tools syntax)
                                           (setq flycheck-python-pycompile-executable "python3")
                                           (flycheck-mode 1)
                                           (flycheck-select-checker 'python-ruff)))))

;; Ruff 格式化工具配置（当不使用 ruff LSP 时）
(unless (modulep! :lang python +ruff)
  (defun python-ruff-format-buffer ()
    "Format current buffer using ruff."
    (interactive)
    (when (executable-find "ruff")
      (let ((original-point (point)))
        (shell-command-on-region
         (point-min) (point-max)
         "ruff format --stdin-filename=-"
         nil t)
        (goto-char original-point))))

  (defun python-ruff-check-buffer ()
    "Check current buffer using ruff."
    (interactive)
    (when (executable-find "ruff")
      (shell-command
       (format "ruff check %s" (shell-quote-argument (buffer-file-name))))))

  ;; Ruff 自动格式化 hook
  (defun python-setup-ruff-format-on-save ()
    "Setup ruff format on save."
    (when (and (executable-find "ruff")
               (derived-mode-p 'python-mode))
      (add-hook 'before-save-hook #'python-ruff-format-buffer nil t)))

  ;; 启用 ruff 自动格式化
  (add-hook 'python-mode-hook #'python-setup-ruff-format-on-save))

;; Python 模式键绑定
(map! :localleader
      :map python-mode-map
      (:prefix ("f" . "format")
       :desc "Ruff format" "f" (if (modulep! :lang python +ruff)
                                   #'lsp-format-buffer
                                 #'python-ruff-format-buffer)
       :desc "Ruff check" "c" (if (modulep! :lang python +ruff)
                                  #'lsp-workspace-restart
                                #'python-ruff-check-buffer)))

;; Pytest 测试框架
(use-package! pytest
  :after python
  :config
  (map! :localleader
        :map python-mode-map
        (:prefix ("t" . "test")
         :desc "Test all" "a" #'pytest-all
         :desc "Test module" "m" #'pytest-module
         :desc "Test one" "o" #'pytest-one
         :desc "Test function" "f" #'pytest-pdb-one
         :desc "Last failed" "l" #'pytest-last-failed))) 
