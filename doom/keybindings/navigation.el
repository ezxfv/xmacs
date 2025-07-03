;;; keybindings/navigation.el -*- lexical-binding: t; -*-

;; 搜索功能
(map! [remap swiper] #'swiper-isearch)

(map!
 "C-s"     #'+default/search-buffer
 )

;; Leader 导航快捷键
(map! :leader
      "/" #'+default/search-project
      "SPC" #'consult-projectile-find-file
      :desc "Open any file" "a" #'ido-find-file
      )

;; 代码导航
(map!
 :desc "Go function header"     :n "g[" #'beginning-of-defun
 :desc "Go function end"        :n "g]" #'end-of-defun
 :desc "Find definition"        :n "gd" #'+lookup/definition
 :desc "Find reference"         :n "gr" #'+lookup/references
 :desc "Find implementation"    :n "gi" #'+lookup/implementations
 :desc "Go back find point"     :n "gb" #'xref-pop-marker-stack
 )

;; LSP 导航增强
(map! [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
      [remap xref-find-references] #'lsp-ui-peek-find-references)

;; Tab 导航
(map!
 :desc "Goto tab in group"      :nv "mt" #'my/switch-to-tab-in-group
 ) 