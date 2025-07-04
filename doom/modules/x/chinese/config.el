;;; modules/x/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pangu-spacing
  :config
  (global-pangu-spacing-mode 1))

(use-package! cnfonts
  :config
  ;; 键位绑定已迁移到 keybindings/applications.el
  )

(use-package! valign
  :after org
  :config
  (add-hook 'org-mode-hook #'valign-mode))

;; pyim 配置
(when (modulep! :input chinese)
  (after! pyim
    (if (display-graphic-p)
        (setq pyim-page-tooltip 'posframe)
      (setq pyim-page-tooltip 'popup))
    (setq pyim-isearch-mode 1
          default-input-method "pyim"
          pyim-dicts `((:name greatdict :file ,(concat doom-user-dir "vendor/pyim/pyim-greatdict.pyim"))
                       (:name sogou :file ,(concat doom-user-dir "vendor/pyim/sogou.pyim")))
          pyim-default-scheme 'quanpin
          pyim-english-input-switch-functions '(pyim-probe-isearch-mode)
          pyim-page-length 6)))