;;; x/english/config.el -*- lexical-binding: t; -*-

(use-package! deno-bridge)

(use-package! sdcv
  :init
  ;; download url: http://download.huzheng.org/zh_CN/
  (setq sdcv-dictionary-simple-list        ;; a simple dictionary list
        '("计算机词汇"
          "英汉汉英专业词典"))
  (setq sdcv-dictionary-complete-list      ;; a complete dictionary list
        '("牛津英汉双解美化版"
          "计算机词汇"
          "牛津现代英汉双解词典"
          "英汉汉英专业词典"
          "21世纪双语科技词典"
          "牛津高阶英汉双解"
          ))
  (setq sdcv-dictionary-data-dir (concat doom-user-dir "vendor/dicts/"))   ;; set local sdcv dict to search word
  )

;; (use-package! company-english-helper
;;   :init
;;   (setq company-english-helper-fuzz-search-p t)
;;   (add-hook! '(text-mode-hook
;;                org-mode-hook
;;                latex-mode-hook
;;                markdown-mode-hook)
;;              #'toggle-company-english-helper)
;;   (map!
;;    :desc "Translate helper" :nvi "C-c C-t" #'toggle-company-english-helper))

;; (use-package! insert-translated-name
;;   :init
;;   (setq insert-translated-name-camel-style-mode-list '(go-mode latex-mode))
;;   (setq insert-translated-name-underline-style-mode-list '(python-mode ruby-mode))
;;   (setq insert-translated-name-translate-engine 'youdao)
;;   (map!
;;    :desc "Translate insert" :nvi "C-c C-e" #'insert-translated-name-insert))
