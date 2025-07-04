;;; modules/x/search-nav/config.el -*- lexical-binding: t; -*-

(use-package! color-rg
  :config
  (setq color-rg-kill-temp-buffer-timeout 3)
  (map! :leader
        (:prefix ("s" . "search")
         :desc "Color rg" "g" #'color-rg-search-symbol
         :desc "Color rg input" "G" #'color-rg-search-input)))

(use-package! consult-projectile
  :config
  (map! :leader
        :desc "Find project file" "p f" #'consult-projectile-find-file
        :desc "Find project dir" "p d" #'consult-projectile-find-dir
        :desc "Switch project buffer" "p b" #'consult-projectile-switch-to-buffer
        :desc "Search project" "p s" #'consult-projectile-ripgrep))

;; 更好的 ripgrep 前端
(use-package! deadgrep
  :config
  (map! :leader
        (:prefix ("s" . "search")
         :desc "Deadgrep" "d" #'deadgrep)))

;; Projectile + Ivy 集成
(use-package! counsel-projectile
  :config
  (map! :leader
        (:prefix ("p" . "project")
         :desc "Counsel find file" "F" #'counsel-projectile-find-file
         :desc "Counsel find dir" "D" #'counsel-projectile-find-dir
         :desc "Counsel switch buffer" "B" #'counsel-projectile-switch-to-buffer
         :desc "Counsel ag" "A" #'counsel-projectile-ag)))

;; 可编辑的 grep 结果
(use-package! wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; 搜索计数显示
(use-package! anzu
  :config
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil)
  (map! [remap query-replace] #'anzu-query-replace
        [remap query-replace-regexp] #'anzu-query-replace-regexp))

;; ripgrep 集成
(use-package! rg
  :config
  (rg-enable-default-bindings)
  (setq rg-group-result t)
  (setq rg-show-columns nil)
  (map! :leader
        (:prefix ("s" . "search")
         :desc "Rg" "r" #'rg
         :desc "Rg literal" "R" #'rg-literal))) 