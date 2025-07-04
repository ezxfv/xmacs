;;; modules/x/dev-utils/config.el -*- lexical-binding: t; -*-

;; 加载工具函数
(load! "utils")

(use-package! gotest
  :after go-mode
  :config
  (setq go-test-verbose t)
  ;; 键位绑定已迁移到 keybindings/development.el
  )

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

;; Kubernetes 管理工具
(use-package! kubernetes
  :commands kubernetes-overview
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600)
  ;; 键位绑定已迁移到 keybindings/development.el
  )

;; K8s YAML 增强
(use-package! k8s-mode
  :hook (yaml-mode . k8s-mode)
  :config
  (setq k8s-search-documentation-browser-function 'browse-url))

;; YAML 模式增强
(use-package! yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)
              (setq yaml-indent-offset 2))))