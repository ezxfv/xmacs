;;; modules/x/search-nav/config.el -*- lexical-binding: t; -*-

(use-package! color-rg
  :config
  (setq color-rg-kill-temp-buffer-timeout 3)
  ;; 键位绑定已迁移到 keybindings/navigation.el
  )

(use-package! consult-projectile
  :config
  ;; 键位绑定已迁移到 keybindings/navigation.el
  )

;; 更好的 ripgrep 前端
(use-package! deadgrep
  :config
  ;; 键位绑定已迁移到 keybindings/navigation.el
  )

;; Projectile + Ivy 集成
(use-package! counsel-projectile
  :config
  ;; 键位绑定已迁移到 keybindings/navigation.el
  )

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
  ;; 键位绑定已迁移到 keybindings/navigation.el
  )

;; ripgrep 集成
(use-package! rg
  :config
  (rg-enable-default-bindings)
  (setq rg-group-result t)
  (setq rg-show-columns nil)
  ;; 键位绑定已迁移到 keybindings/navigation.el
  )