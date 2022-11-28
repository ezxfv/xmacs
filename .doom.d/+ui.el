(if (display-graphic-p)
    ;; gui mode
    (progn
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      (plist-put +popup-defaults :modeline t)
      (setq fancy-splash-image "~/.doom.d/banner/hack.png")
      (setq doom-theme 'doom-snazzy)
      (setq doom-themes-treemacs-theme "doom-colors")
      ;; (after! evil
      ;;   (custom-set-faces '(cursor ((t (:background "#009999" :foreground "#222222")))))
      ;;   (setq evil-normal-state-cursor '(box "dark red")
      ;;         red-insert-state-cursor '(bar "black")
      ;;         evil-visual-state-cursor '(hollow "orange"))
      ;;   )
      ;; (after! flycheck
      ;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
      )
  ;; terminal mode
  (setq doom-theme 'doom-dark+)
  )

;; (after! dap-mode
;;   (custom-set-faces
;;    '(dap-ui-pending-breakpoint-face ((t (:background "dim gray"))))
;;    '(dap-ui-breakpoint-verified-fringe ((t (:foreground "red" :weight bold))))
;;    '(dap-ui-verified-breakpoint-face ((t (:background "red" :weight bold)))))
;;   )
