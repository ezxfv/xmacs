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
  (map! :nv "C-S-j" #'move-text-down
        :nv "C-S-k" #'move-text-up))

(use-package! visual-regexp
  :config
  (map! :nv "C-c r" #'vr/replace
        :nv "C-c q" #'vr/query-replace))

(use-package! visual-regexp-steroids
  :after visual-regexp)

(use-package! crux
  :config
  (map! [remap move-beginning-of-line] #'crux-move-beginning-of-line
        [remap kill-line] #'crux-smart-kill-line
        "C-c n" #'crux-cleanup-buffer-or-region
        "C-c f" #'crux-recentf-find-file
        "C-c d" #'crux-duplicate-current-line-or-region))

(use-package! string-inflection
  :config
  (map! :leader "c ~" #'string-inflection-all-cycle))

(use-package! thing-edit)

;; 多点编辑
(use-package! iedit
  :config
  (map! "C-;" #'iedit-mode))

;; 快速跳转
(use-package! avy
  :config
  (setq avy-timeout-seconds 0.5)
  (map! "C-:" #'avy-goto-char
        "C-'" #'avy-goto-char-2
        "M-g f" #'avy-goto-line
        "M-g w" #'avy-goto-word-1))

;; 窗口管理
(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (map! "M-o" #'ace-window))

;; 智能选择扩展
(use-package! expand-region
  :config
  (map! "C-=" #'er/expand-region
        "C--" #'er/contract-region))

;; 括号/引号操作
(use-package! embrace
  :config
  (map! "C-," #'embrace-commander))

;; 多光标编辑
(use-package! multiple-cursors
  :config
  (map! :prefix "C-c m"
        "e" #'mc/edit-lines
        "a" #'mc/mark-all-like-this
        "n" #'mc/mark-next-like-this
        "p" #'mc/mark-previous-like-this
        "r" #'mc/mark-all-in-region))

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
  (map! "C-x u" #'undo-tree-visualize))