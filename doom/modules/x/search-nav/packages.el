;;; modules/x/search-nav/packages.el -*- lexical-binding: t; -*-

(package! color-rg
  :recipe (:host github
           :repo "manateelazycat/color-rg"))

(when (modulep! :completion vertico)
  (package! consult-projectile
    :recipe (:host gitlab :repo "OlMon/consult-projectile" :branch "master")))

;; 搜索和导航增强
(package! deadgrep)                      ; 更好的 ripgrep 前端
(package! counsel-projectile)            ; Projectile + Ivy 集成
(package! wgrep)                         ; 可编辑的 grep 结果
(package! anzu)                          ; 搜索计数显示
(package! rg)                           ; ripgrep 集成 