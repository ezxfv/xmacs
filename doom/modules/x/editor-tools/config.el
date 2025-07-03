;;; modules/x/editor-tools/config.el -*- lexical-binding: t; -*-

;; 加载工具函数
(load! "utils")

(use-package! smart-hungry-delete
  :config
  (smart-hungry-delete-add-default-hooks))

(use-package! move-text
  :config
  (move-text-default-bindings))

(use-package! visual-regexp
  :config
  (map! :nv "C-c r" #'vr/replace
        :nv "C-c q" #'vr/query-replace))

(use-package! visual-regexp-steroids
  :after visual-regexp)

(use-package! crux
  :config
  (map! :nv "C-c o" #'crux-open-with
        :nv "C-c u" #'crux-view-url))

(use-package! string-inflection
  :config
  (map! :nv "C-c C-s" #'string-inflection-all-cycle))

(use-package! thing-edit
  :config
  (map! :nv "C-c t" #'thing-edit-sexp)) 