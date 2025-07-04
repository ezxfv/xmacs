;;; keybindings/editing.el -*- lexical-binding: t; -*-

;; 基础编辑操作
(map!
 "C-c d"     (cmd! (previous-line)
                   (kill-line)
                   (forward-line))
 :niv      "C-e"     #'move-to-end-of-line
 )

;; Multiple cursors 多光标编辑
(map!
 ;; multiple cursors
 "C->"     #'mc/mark-next-like-this
 "C-<"     #'mc/mark-previous-like-this
 "C-c C-<" #'mc/mark-all-like-this
 "C-S-c C-S-c" #'mc/edit-lines
 "C-S-c 0" #'mc/insert-numbers
 "C-S-c 1" #'mc/insert-letters
 "C-S-c s" #'mc/mark-all-in-region
 "C-S-c S" #'mc/mark-all-in-region-regexp
 )

;; Evil 模式下的编辑增强
(map!
 :desc "Delete parens"          :n "z-" #'sp-splice-sexp
 :desc "Wrap with markup"       :nv "z." #'emmet-wrap-with-markup
 :desc "Increase number"        :n "+"  #'evil-numbers/inc-at-pt
 :desc "Decrease number"        :n "-"  #'evil-numbers/dec-at-pt
 :desc "Mark fun"               :nv "mf" #'mark-defun
 :desc "Delete to begin"        :nv "mh" #'evil-delete-line-forward
 :desc "Multiedit match all"    :nv "mma" #'evil-multiedit-match-all
 :desc "Comment or Uncomment Line" :nv ";" #'evilnc-comment-or-uncomment-lines
 )

;; 编辑工具增强功能
(when (modulep! :x editor-tools)
  ;; 文本移动
  (map! :nv "C-S-j" #'move-text-down
        :nv "C-S-k" #'move-text-up)
  
  ;; 正则替换
  (map! :nv "C-c r" #'vr/replace
        :nv "C-c q" #'vr/query-replace)
  
  ;; Crux 智能编辑
  (map! [remap move-beginning-of-line] #'crux-move-beginning-of-line
        [remap kill-line] #'crux-smart-kill-line
        "C-c n" #'crux-cleanup-buffer-or-region
        "C-c f" #'crux-recentf-find-file
        "C-c d" #'crux-duplicate-current-line-or-region)
  
  ;; 字符串变换
  (map! :leader "c ~" #'string-inflection-all-cycle)
  
  ;; 多点编辑
  (map! "C-;" #'iedit-mode)
  
  ;; 快速跳转
  (map! "C-:" #'avy-goto-char
        "C-'" #'avy-goto-char-2
        "M-g f" #'avy-goto-line
        "M-g w" #'avy-goto-word-1)
  
  ;; 窗口管理
  (map! "M-o" #'ace-window)
  
  ;; 智能选择扩展
  (map! "C-=" #'er/expand-region
        "C--" #'er/contract-region)
  
  ;; 括号/引号操作
  (map! "C-," #'embrace-commander)
  
  ;; 多光标编辑增强
  (map! :prefix "C-c m"
        "e" #'mc/edit-lines
        "a" #'mc/mark-all-like-this
        "n" #'mc/mark-next-like-this
        "p" #'mc/mark-previous-like-this
        "r" #'mc/mark-all-in-region)
  
  ;; 撤销树可视化
  (map! "C-x u" #'undo-tree-visualize))

;; 撤销树可视化
(when (modulep! :x undo-tree)
  (map! :map undo-tree-visualizer-mode-map
        :n "C-g" #'undo-tree-visualizer-quit
        :n "q" #'undo-tree-visualizer-quit
        :n "RET" #'undo-tree-visualizer-set))

;; Copilot 代码补全
(when (modulep! :x copilot)
  (map! :map copilot-completion-map
        "<tab>" #'copilot-accept-completion
        "TAB" #'copilot-accept-completion
        "C-TAB" #'copilot-accept-completion-by-word
        "C-<tab>" #'copilot-accept-completion-by-word
        "C-n" #'copilot-next-completion
        "C-p" #'copilot-previous-completion))