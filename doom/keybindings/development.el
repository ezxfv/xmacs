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