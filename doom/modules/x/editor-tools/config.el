;;; modules/x/editor-tools/config.el -*- lexical-binding: t; -*-

;; 加载工具函数
(load! "utils")

(use-package! smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package! move-text
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

(use-package! visual-regexp
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

(use-package! visual-regexp-steroids
  :after visual-regexp)

(use-package! crux
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

(use-package! string-inflection
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

(use-package! thing-edit)

;; 多点编辑
(use-package! iedit
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

;; 快速跳转
(use-package! avy
  :config
  (setq avy-timeout-seconds 0.5)
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

;; 窗口管理
(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

;; 智能选择扩展
(use-package! expand-region
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

;; 括号/引号操作
(use-package! embrace
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

;; 多光标编辑
(use-package! multiple-cursors
  :config
  ;; 键位绑定已迁移到 keybindings/editing.el
  )

;; 智能括号（如果 Doom 没有启用）
(use-package! smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; 撤销树可视化
(use-package! undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  ;; 键位绑定已迁移到 keybindings/editing.el
  )