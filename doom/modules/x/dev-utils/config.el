;;; modules/x/dev-utils/config.el -*- lexical-binding: t; -*-

;; 加载工具函数
(load! "utils")

(use-package! gotest
  :after go-mode
  :config
  (setq go-test-verbose t)
  (map! :localleader
        :map go-mode-map
        (:prefix ("t" . "test")
         :desc "Test function" "f" #'go-test-current-test
         :desc "Test file" "t" #'go-test-current-file
         :desc "Test project" "p" #'go-test-current-project
         :desc "Test coverage" "c" #'go-test-current-coverage)))

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line)))

(use-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))) 