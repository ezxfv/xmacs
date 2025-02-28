;; (after! doom-themes
;;   (load-theme 'doom-nano-light t)
;;   (load-theme 'doom-nano-dark t))

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
      ;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
      ;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
      ;; are the three important ones:
      ;;
      ;; + `doom-font'
      ;; + `doom-variable-pitch-font'
      ;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
      ;;   presentations or streaming.  ;; ;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd ;; font string. You generally only need these two:
      ;;   intsll: /bin/bash -c '$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)'
      (if (getenv "WSL_DISTRO_NAME")
          (progn
            (setq doom-font (font-spec :family "JetBrains Mono" :size 22 :height 1.8)
                  doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 22 :height 1.8)
                  doom-big-font (font-spec :family "JetBrains Mono" :size 22 :height 1.8))	;; are the three important ones:
            (add-hook 'after-init-hook (lambda ()
                                         (text-scale-set 2))))
        (progn
          (setq doom-font (font-spec :family "JetBrains Mono" :size 16 :height 1.2)
                doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 16 :height 1.2)
                doom-big-font (font-spec :family "JetBrains Mono" :size 18 :height 1.2))	;; are the three important ones:
          (add-hook 'after-init-hook (lambda ()
                                       (text-scale-set 1.5)))))
      ;; (after! evil
      ;;   (custom-set-faces '(cursor ((t (:background "#009999" :foreground "#222222")))))
      ;;   (setq evil-normal-state-cursor '(box "dark red")
      ;;         red-insert-state-cursor '(bar "black")
      ;;         evil-visual-state-cursor '(hollow "orange"))
      ;;   )
      ;; (after! flycheck
      ;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
      ;;(load-theme 'doom-nano-light t)
      ;;(load-theme 'doom-nano-dark t)
      ;;(setq doom-theme 'doom-nane-light)
      ;; (use-package! doom-nano-modeline
      ;;   :config
      ;;   (doom-nano-modeline-mode 1)
      ;;   (global-hide-mode-line-mode 1))
      )
  ;; terminal mode
  (setq doom-theme 'tsdh-dark)
  )

;; (after! dap-mode
;;   (custom-set-faces
;;    '(dap-ui-pending-breakpoint-face ((t (:background "dim gray"))))
;;    '(dap-ui-breakpoint-verified-fringe ((t (:foreground "red" :weight bold))))
;;    '(dap-ui-verified-breakpoint-face ((t (:background "red" :weight bold)))))
;;   )
