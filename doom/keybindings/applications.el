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
 )

;; AI 工具快捷键
(when (modulep! :x aidermacs)
  (map! :leader
        (:prefix ("e" . "AI")
         :desc "启动 Aider 会话" "s" #'aidermacs-start
         :desc "添加文件" "a" #'aidermacs-add-file
         :desc "交互式添加文件" "i" #'aidermacs-add-files-interactively
         :desc "移除文件" "d" #'aidermacs-drop-file
         :desc "列出已添加文件" "l" #'aidermacs-list-added-files
         :desc "显示输出历史" "h" #'aidermacs-show-output-history
         :desc "创建临时文件" "c" #'aidermacs-create-session-scratchpad))
  (map! "C-c a i" #'aidermacs-transient-menu))

;; 中文字体调整
(when (modulep! :x chinese)
  (map! :niv "C--" #'cnfonts-decrease-fontsize
        :niv "C-+" #'cnfonts-increase-fontsize))

;; 知识管理 - 书签
(when (modulep! :x knowledge)
  (map! :leader
        :desc "Open bookmarks menu" "m b" #'bmkp-bmenu-list-bookmarks
        :desc "Set bookmark" "m m" #'bmkp-set-bookmark
        :desc "Jump to bookmark" "m j" #'bmkp-jump-to-list))

;; UI 主题相关
(when (modulep! :x ui-theme)
  (map! :leader
        :desc "Toggle crosshairs" "t c" #'crosshairs-mode)
  ;; Mac 平台的 ultra-scroll
  (when IS-MAC
    (map! :leader
          :desc "Check ultra-scroll compatibility" "t u" #'ultra-scroll-check)))

;; 注释掉的功能（保留以备将来使用）
;; "C-c y"       #'youdao-dictionary-search-at-point+