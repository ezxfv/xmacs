;;; keybindings/applications.el -*- lexical-binding: t; -*-

;; Org 相关工具和函数禁用
(map! [remap org-capture] nil)

;; 应用功能快捷键
(map!
 ;; Org Mac Chrome 集成
 "C-c a c"     #'org-mac-chrome-insert-frontmost-url
 
 ;; Crux 实用工具
 "C-c o"       #'crux-open-with
 "C-c u"       #'crux-view-url

 ;; JSON 格式化
 "C-c C-f"     #'json-mode-beautify
 
 ;; PlantUML 预览
 "C-c C-r"     #'plantuml-preview-region
 
 ;; 字体调整
 :niv      "C--"     #'cnfonts-decrease-fontsize
 :niv      "C-+"     #'cnfonts-increase-fontsize
 )

;; 注释掉的功能（保留以备将来使用）
;; "C-c y"       #'youdao-dictionary-search-at-point+ 