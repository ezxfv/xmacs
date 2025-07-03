;;; modules/x/ui-theme/config.el -*- lexical-binding: t; -*-

;; 加载工具函数
(load! "utils")

(use-package! doom-nano-modeline
  :config
  ;; 可以根据需要启用
  ;; (doom-nano-modeline-mode 1)
  ;; (global-hide-mode-line-mode 1)
  )

(use-package! crosshairs
  :config
  (map! :leader
        :desc "Toggle crosshairs" "t c" #'crosshairs-mode))

;; UI 配置从 +ui.el 移过来
(if (display-graphic-p)
    ;; gui mode
    (progn
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      (plist-put +popup-defaults :modeline t)
      ;; (setq fancy-splash-image "~/.doom.d/banner/hack.png")
      (setq doom-theme 'tsdh-light)
      (setq doom-themes-treemacs-theme "doom-colors")
      (setq pixel-scroll-precision-mode t)
      (setq pixel-scroll-mode t)
      ;; WSL 环境特殊处理
      (if (getenv "WSL_DISTRO_NAME")
          (progn
            (setq doom-font (font-spec :family "JetBrains Mono" :size 22 :height 1.8)
                  doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 22 :height 1.8)
                  doom-big-font (font-spec :family "JetBrains Mono" :size 22 :height 1.8))
            (add-hook 'after-init-hook (lambda ()
                                         (text-scale-set 2))))
        (progn
          (setq doom-font (font-spec :family "JetBrains Mono" :size 16 :height 1.2)
                doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 16 :height 1.2)
                doom-big-font (font-spec :family "JetBrains Mono" :size 18 :height 1.2))
          (add-hook 'after-init-hook (lambda ()
                                       (text-scale-set 1.5))))))
  ;; terminal mode
  (setq doom-theme 'tsdh-dark)) 