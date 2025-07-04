;;; modules/x/lang/+go.el -*- lexical-binding: t; -*-
;; Go settings

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer -100 t)
  (add-hook 'before-save-hook #'lsp-organize-imports -99 t))

(defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))

(defun eglot-go-install-save-hooks ()
  (add-hook 'before-save-hook #'gofmt-before-save -100 t)
  (add-hook 'before-save-hook #'eglot-format-buffer -90 t)
  (add-hook 'before-save-hook #'+eglot-organize-imports -80 t))

(when (modulep! :lang go)
  (if (modulep! :tools lsp +eglot)
      (progn
        (after! go-mode
          (setq flycheck-golangci-lint-fast t)
          (setq flycheck-golangci-lint-config (concat doom-user-dir "vendor/golangci.yml"))
          (add-hook 'go-mode-hook #'eglot-go-install-save-hooks)))
    (after! go-mode
      (setq flycheck-golangci-lint-fast t)
      (setq go-test-verbose t)
      (setq flycheck-golangci-lint-config (concat doom-user-dir "vendor/golangci.yml"))
      (add-hook 'go-mode-hook 'lsp-deferred)
      (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))))

;; Go Tag - 自动添加/移除 struct tag
(use-package! go-tag
  :after go-mode
  :config
  (setq go-tag-args '("-transform" "camelcase"))
  (map! :localleader
        :map go-mode-map
        (:prefix ("r" . "refactor")
         :desc "Add tags" "a" #'go-tag-add
         :desc "Remove tags" "r" #'go-tag-remove)))

;; Go Fill Struct - 自动填充结构体字段
(use-package! go-fill-struct
  :after go-mode
  :config
  (map! :localleader
        :map go-mode-map
        (:prefix ("r" . "refactor")
         :desc "Fill struct" "f" #'go-fill-struct))) 