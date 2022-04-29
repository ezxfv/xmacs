(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-tomorrow-day)
;;(setq doom-theme 'doom-material)
(if (display-graphic-p)
    ;; gui mode
    (progn
      (setq doom-theme 'doom-solarized-light)
      (setq doom-themes-treemacs-theme "doom-colors")
      ;; (after! evil
      ;;   (custom-set-faces '(cursor ((t (:background "#009999" :foreground "#222222")))))
      ;;   (setq evil-normal-state-cursor '(box "dark red")
      ;;         red-insert-state-cursor '(bar "black")
      ;;         evil-visual-state-cursor '(hollow "orange"))
      ;;   )
      (after! flycheck
        (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
      )
  ;; terminal mode
  (setq doom-theme 'doom-dark+)
  (setq doom-themes-treemacs-theme "doom-colors")
  )

(plist-put +popup-defaults :modeline t)

;; (after! ivy-posframe
;;   (custom-set-faces '(ivy-posframe ((t (:background "#4B4B4B" :foreground "#00CC66")))))
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-window-center))))

;; (after! dap-mode
;;   (custom-set-faces
;;    '(dap-ui-pending-breakpoint-face ((t (:background "dim gray"))))
;;    '(dap-ui-breakpoint-verified-fringe ((t (:foreground "red" :weight bold))))
;;    '(dap-ui-verified-breakpoint-face ((t (:background "red" :weight bold)))))
;;   )

(provide 'xui)
