;;; x/english/config.el -*- lexical-binding: t; -*-

(use-package! deno-bridge)

(use-package! sdcv
  :init
  ;; download url: http://download.huzheng.org/zh_CN/
  (setq sdcv-dictionary-simple-list        ;; a simple dictionary list
        '("朗道英汉字典5.0"
          "计算机词汇"))
  (setq sdcv-dictionary-complete-list      ;; a complete dictionary list
        '("朗道英汉字典5.0"
          "牛津高阶英汉双解"
          "牛津现代英汉双解词典"
          "牛津英汉双解美化版"
          "Webster's Revised Unabridged Dictionary (1913)"
          "计算机词汇"
          "中国大百科全书2.0版"
          ))
  (setq sdcv-dictionary-data-dir "~/stardict/dic")   ;; set local sdcv dict to search word
  )

(use-package! company-english-helper
  :init
  (setq company-english-helper-fuzz-search-p t)
  (add-hook! '(text-mode-hook
               org-mode-hook
               latex-mode-hook
               markdown-mode-hook)
             #'toggle-company-english-helper)
  (map!
   :desc "Translate helper" :nvi "C-c C-t" #'toggle-company-english-helper))

(use-package! insert-translated-name
  :init
  (setq insert-translated-name-camel-style-mode-list '(go-mode latex-mode))
  (setq insert-translated-name-underline-style-mode-list '(python-mode ruby-mode))
  (setq insert-translated-name-translate-engine 'youdao)
  (map!
   :desc "Translate insert" :nvi "C-c C-e" #'insert-translated-name-insert))
