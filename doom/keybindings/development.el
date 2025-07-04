;;; keybindings/development.el -*- lexical-binding: t; -*-

;; 代码检查和错误处理
(map!
 :desc "List fly check errors"  :n "fl" #'list-flycheck-errors
 :desc "Next flycheck error"    :n "fn" #'flycheck-next-error
 :desc "Previous flycheck error" :n "fp" #'flycheck-previous-error
 :desc "Select checker"         :n "ms" #'flycheck-select-checker
 )

;; 代码格式化和文件操作
(map!
 :desc "Format buffer"          :n "fb" #'+format/buffer
 :desc "Save buffer"            :n "fs" #'save-buffer
 )

;; 实用功能
(map!
 :desc "M-x"                    :n "mx" #'execute-extended-command
 )

;; Go 开发工具
(when (modulep! :x dev-utils)
  ;; Go 测试快捷键
  (map! :localleader
        :map go-mode-map
        (:prefix ("t" . "test")
         :desc "Test function" "f" #'go-test-current-test
         :desc "Test file" "t" #'go-test-current-file
         :desc "Test project" "p" #'go-test-current-project
         :desc "Test coverage" "c" #'go-test-current-coverage))
  
  ;; Kubernetes 管理
  (map! :leader
        (:prefix ("k" . "kubernetes")
         :desc "Overview" "o" #'kubernetes-overview
         :desc "Display pod" "p" #'kubernetes-display-pod
         :desc "Display config" "c" #'kubernetes-display-config-map
         :desc "Display secret" "s" #'kubernetes-display-secret)))

;; LSP 和 TreeMacs 集成
(when (modulep! :x lang)
  (when (modulep! :ui treemacs)
    (global-set-key (kbd "C-c C-o") (lambda () (interactive) (lsp-treemacs-call-hierarchy t)))
    (global-set-key (kbd "C-c C-i") (lambda () (interactive) (lsp-treemacs-call-hierarchy nil)))))