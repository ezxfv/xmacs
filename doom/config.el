;; private config
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind
  ( :map completion-preview-active-mode-map
         ("M-n" . completion-preview-next-candidate)
         ("M-p" . completion-preview-prev-candidate)))

;; Some functionality uses this to identify you, e.g. GPG onfiguration, email
;; clients, file templates and snippets.
(setq user-full-name "Eden Zhong"
      user-mail-address "edenzhong7@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;   install: /bin/bash -c '$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)'
(setq doom-font (font-spec :family "JetBrainsMono" :size 16)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono" :size 14))
;(setq doom-font (font-spec :family "Monaspace Neon" :size 14)
;      doom-variable-pitch-font (font-spec :family "Monaspace Neon" :size 12))

;; 修复modeline图标显示
(after! doom-modeline
  (setq doom-modeline-height 28)  ;; 进一步增加modeline高度
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project))

;; 确保treemacs图标正确显示
(after! treemacs
  (setq treemacs-no-png-images nil)
  (setq treemacs-width 30))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(after! lsp-mode
  (delete 'lsp-terraform lsp-client-packages))

;; General configuration
(setq-default history-length 999)
(setq-default prescient-history-length 1000)
(setq native-comp-async-report-warnings-errors nil)

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed 't)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 2)

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

;; Custom module config
(when (modulep! :completion company)
  (after! company
    (setq company-idle-delay 0
          company-minimum-prefix-length 2
          company-show-quick-access t
          company-insertion-triggers t
          company-dabbrev-downcase nil
          company-selection-wrap-around t)))

(when (modulep! :checkers spell)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_US"))

(when (modulep! :term eshell)
  (use-package! esh-autosuggest
    :config
    (add-hook 'eshell-mode-hook #'esh-autosuggest-mode -100)))

(when (modulep! :x doom-meow)
  (setq meow-use-clipboard t)
  (setq doom-localleader-alt-key "C-l")
  (map! :map meow-leader-keymap
        "l" #'meow-keypad-start))

(after! evil
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (setq evil-fold-list nil)) ;; 禁用 Doom 的默认折叠机制

;; UI 配置
(if (display-graphic-p)
    ;; gui mode
    (progn
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      (plist-put +popup-defaults :modeline t)
      ;; (setq fancy-splash-image "~/.doom.d/banner/hack.png")
      (setq doom-theme 'tsdh-light)
      (setq doom-themes-treemacs-theme "doom-colors"))

  ;; terminal mode
  (setq doom-theme 'tsdh-dark))

(after! gcmh
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 32 1024 1024 1024)  ;; 32GB
        gcmh-low-cons-threshold  (* 2 1024 1024 1024)   ;; 2GB
        gc-cons-percentage 0.6))

;; 失焦/空闲时收一收，减少回到 Emacs 的首卡
(add-hook 'focus-out-hook #'garbage-collect)

;; 迷你缓冲区输入时尽量不 GC（可选）
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook  (lambda () (setq gc-cons-threshold gcmh-high-cons-threshold)))

;; 加载键盘绑定配置
(load! "keybindings/keybindings")
