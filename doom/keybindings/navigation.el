;;; keybindings/navigation.el -*- lexical-binding: t; -*-

;; 导航和搜索快捷键
(map!
 ;; 文件和项目导航
 :desc "Find file in project"   :n "ff" #'projectile-find-file
 :desc "Find recent files"      :n "fr" #'recentf-open-files
 :desc "Switch to buffer"       :n "bb" #'switch-to-buffer
 :desc "Kill buffer"            :n "bk" #'kill-buffer
 :desc "List buffers"           :n "bl" #'list-buffers
 
 ;; 项目管理
 :desc "Switch project"         :n "pp" #'projectile-switch-project
 :desc "Find in project"        :n "sp" #'projectile-ripgrep
 :desc "Replace in project"     :n "rp" #'projectile-replace
 
 ;; 代码导航
 :desc "Go to definition"       :n "gd" #'lsp-find-definition
 :desc "Go to references"       :n "gr" #'lsp-find-references
 :desc "Go to implementation"   :n "gi" #'lsp-find-implementation
 :desc "Go back"                :n "gb" #'pop-tag-mark
 
 ;; 搜索功能
 :desc "Search symbol"          :n "ss" #'swiper-isearch-thing-at-point
 :desc "Search in buffer"       :n "sb" #'swiper
 :desc "Search in project"      :n "sp" #'counsel-projectile-rg
 )

;; 搜索和导航增强功能
(when (modulep! :x search-nav)
  ;; Color RG 搜索
  (map! :leader
        (:prefix ("s" . "search")
         :desc "Color rg" "g" #'color-rg-search-symbol
         :desc "Color rg input" "G" #'color-rg-search-input
         :desc "Deadgrep" "d" #'deadgrep
         :desc "Rg" "r" #'rg
         :desc "Rg literal" "R" #'rg-literal))
  
  ;; Consult Projectile 集成
  (map! :leader
        :desc "Find project file" "p f" #'consult-projectile-find-file
        :desc "Find project dir" "p d" #'consult-projectile-find-dir
        :desc "Switch project buffer" "p b" #'consult-projectile-switch-to-buffer
        :desc "Search project" "p s" #'consult-projectile-ripgrep)
  
  ;; Counsel Projectile 集成
  (map! :leader
        (:prefix ("p" . "project")
         :desc "Counsel find file" "F" #'counsel-projectile-find-file
         :desc "Counsel find dir" "D" #'counsel-projectile-find-dir
         :desc "Counsel switch buffer" "B" #'counsel-projectile-switch-to-buffer
         :desc "Counsel ag" "A" #'counsel-projectile-ag))
  
  ;; Anzu 搜索计数
  (map! [remap query-replace] #'anzu-query-replace
        [remap query-replace-regexp] #'anzu-query-replace-regexp))