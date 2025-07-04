;;; modules/x/editor-tools/packages.el -*- lexical-binding: t; -*-

(package! smart-hungry-delete)
(package! move-text)
(package! visual-regexp)
(package! visual-regexp-steroids
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
(package! crux)
(package! string-inflection)
(package! thing-edit
  :recipe (:host github
           :repo "manateelazycat/thing-edit"))

;; 高效编辑增强工具
(package! iedit)                          ; 多点编辑
(package! avy)                           ; 快速跳转
(package! ace-window)                    ; 窗口快速切换  
(package! expand-region)                 ; 智能选择扩展
(package! embrace)                       ; 括号/引号操作
(package! multiple-cursors)              ; 多光标编辑
(package! smartparens)                   ; 智能括号
(package! undo-tree)                     ; 撤销树可视化 