;;; keybindings/editing.el -*- lexical-binding: t; -*-

;; 基础编辑操作
(map!
 "C-c d"     (cmd! (previous-line)
                   (kill-line)
                   (forward-line))
 :niv      "C-e"     #'move-to-end-of-line
 :niv      "C-="     #'er/expand-region
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

;; 注意：visual-regexp 的快捷键 (C-c r, C-c q) 已移至 editor-tools 模块中统一管理 