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

;; 平台特定的滚动配置
(cond
 ;; Mac 平台使用 ultra-scroll
 (IS-MAC
  (use-package! ultra-scroll
    :config
    ;; 启用 ultra-scroll 模式
    (ultra-scroll-mode 1)
    
    ;; 可选配置：调整滚动行为
    ;; (setq ultra-scroll-wheel-ratio 0.5)  ; 调整滚动灵敏度 (默认 1.0)
    ;; (setq ultra-scroll-vscroll-ratio 2.0) ; 调整虚拟滚动比例 (默认 1.0)
    (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
          scroll-margin 0)        ; important: scroll-margin>0 not yet supported
    
    ;; 快捷键绑定
    (map! :leader
          :desc "Check ultra-scroll compatibility" "t u" #'ultra-scroll-check)))
 
 ;; 其他平台使用系统级像素滚动
 (t
  ;; 启用内置的像素级滚动
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  
  ;; 为不支持像素滚动的设备启用插值模式
  (when (fboundp 'pixel-scroll-precision-interpolate-mice)
    (setq pixel-scroll-precision-interpolate-mice t))
  
  ;; 基础滚动设置
  (setq scroll-conservatively 101
        scroll-margin 0
        scroll-step 1
        scroll-preserve-screen-position t)
  
  ;; 快捷键绑定
  (map! :leader
        :desc "Toggle pixel scroll precision" "t u" #'pixel-scroll-precision-mode)))

;; UI 配置从 +ui.el 移过来
(if (display-graphic-p)
    ;; gui mode
    (progn
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      (plist-put +popup-defaults :modeline t)
      ;; (setq fancy-splash-image "~/.doom.d/banner/hack.png")
      (setq doom-theme 'tsdh-light)
      (setq doom-themes-treemacs-theme "doom-colors")
      
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