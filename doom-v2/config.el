;;; config.el -*- lexical-binding: t; -*-

;; ─── 1. User Identity ─────────────────────────────────────────
(setq user-full-name "Eden Zhong"
      user-mail-address "edenzhong7@qq.com")

;; ─── 2. Fonts & Display ──────────────────────────────────────
(setq doom-font (font-spec :family "JetBrainsMono" :size 16)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono" :size 14))

(after! doom-modeline
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-with-project))

(after! treemacs
  (setq treemacs-width 30))

(setq display-line-numbers-type nil)

;; Theme: GUI vs terminal
(if (display-graphic-p)
    (progn
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      (setq doom-theme 'tsdh-light))
  (setq doom-theme 'tsdh-dark))

;; ─── 3. Performance & GC ─────────────────────────────────────
(setq native-comp-async-report-warnings-errors nil)

(after! gcmh
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 32 1024 1024)   ;; 32 MB
        gcmh-low-cons-threshold  (* 2 1024 1024)    ;; 2 MB
        gc-cons-percentage 0.1))

(add-hook 'focus-out-hook #'garbage-collect)

;; ─── 4. Evil Base Config ─────────────────────────────────────
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

(setq evil-snipe-override-evil-repeat-keys nil)

;; ─── 5. Completion ────────────────────────────────────────────
(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind
  (:map completion-preview-active-mode-map
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate)))

(when (modulep! :completion company)
  (after! company
    (setq company-idle-delay 0
          company-minimum-prefix-length 2
          company-show-quick-access t
          company-selection-wrap-around t)))

;; ─── 6. LSP + lsp-booster ─────────────────────────────────────
(after! lsp-mode
  (setq lsp-idle-delay 0.10
        lsp-auto-configure t
        lsp-enable-imenu t
        lsp-enable-indentation t
        lsp-enable-xref t
        lsp-enable-snippet t
        lsp-signature-auto-activate t
        lsp-enable-file-watchers nil
        lsp-go-use-placeholders nil)

  (add-to-list 'lsp-file-watch-ignored-files
               '("[/\\\\]\\vendor\\'" "[/\\\\]\\third_party\\'" "[/\\\\]\\build\\'"))

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))

  ;; lsp-booster -- accelerate JSON parsing and command dispatch
  (when (executable-find "emacs-lsp-booster")
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      (or (when (equal (following-char) ?#)
            (let ((bytecode (read (current-buffer))))
              (when (byte-code-function-p bytecode)
                (funcall bytecode))))
          (apply old-fn args)))

    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)
                 (not (file-remote-p default-directory))
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))
                 (executable-find "emacs-lsp-booster"))
            (progn
              (when-let ((command-from-exec-path (executable-find (car orig-result))))
                (setcar orig-result command-from-exec-path))
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))

    (advice-add 'lsp-resolve-final-command :around
                #'lsp-booster--advice-final-command)))

;; DAP debugging
(use-package! dap-mode
  :config
  (dap-auto-configure-mode))

(use-package! dap-dlv-go
  :after dap-mode
  :config
  (require 'dap-dlv-go))

;; ─── 7. Go Config ─────────────────────────────────────────────
(after! go-mode
  (add-hook 'before-save-hook #'lsp-format-buffer -100 t)
  (add-hook 'before-save-hook #'lsp-organize-imports -99 t)

  (setq go-test-verbose t
        flycheck-golangci-lint-fast t)

  (add-hook 'go-mode-hook #'lsp-deferred))

(use-package! gotest
  :after go-mode
  :config (setq go-test-verbose t))

(use-package! go-tag
  :after go-mode
  :config (setq go-tag-args '("-transform" "camelcase")))

(use-package! go-fill-struct
  :after go-mode)

;; ─── 8. Python Config ─────────────────────────────────────────
(after! python-mode
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i"
        doom-modeline-env-python-executable "python3")

  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")

  (when (getenv "CONDA_PREFIX_1")
    (setq conda-anaconda-home (getenv "CONDA_PREFIX_1"))
    (conda-env-autoactivate-mode 1))

  (add-hook 'python-mode-local-vars-hook
            (lambda ()
              (semantic-mode 1)
              (when (modulep! :tools lsp)
                (lsp-deferred)))))

(use-package! pytest
  :after python)

;; ─── 9. TypeScript / JavaScript ───────────────────────────────
(after! typescript-mode
  (add-hook 'typescript-mode-hook #'lsp-deferred))

(after! web-mode
  (when (derived-mode-p 'web-mode)
    (when (string-match-p "\\.tsx\\'" (or (buffer-file-name) ""))
      (lsp-deferred))))

;; ─── 10. Docker / Kubernetes / Helm ───────────────────────────
(after! dockerfile-mode
  (add-hook 'dockerfile-mode-hook #'lsp-deferred))

(use-package! k8s-mode
  :hook (yaml-mode . k8s-mode)
  :config
  (setq k8s-search-documentation-browser-function 'browse-url))

(use-package! kubernetes
  :commands kubernetes-overview
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;; Helm: .tpl files are Go templates, .gotmpl for explicit naming
(add-to-list 'auto-mode-alist '("\\helmfile\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.gotmpl\\'" . go-mode))

;; ─── 11. AI / agent-shell ─────────────────────────────────────
(use-package! agent-shell
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :init
  ;; Anthropic API key from environment variable
  (when (getenv "ANTHROPIC_API_KEY")
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication
           :api-key (getenv "ANTHROPIC_API_KEY")))
    (setq agent-shell-preferred-agent-config
          (agent-shell-anthropic-make-claude-code-config))
    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables
           "ANTHROPIC_API_KEY"
           (getenv "ANTHROPIC_API_KEY")))))

;; ─── 12. Org-mode ─────────────────────────────────────────────
(setq org-directory "~/org/")

(after! org
  (setq org-startup-indented t
        org-log-done 'time))

(when (featurep! :lang org +roam2)
  (use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  (use-package! org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :config
    (setq orb-templates
          '(("r" "ref" plain (function org-roam-capture--get-point)
             ""
             :file-name "${slug}"
             :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:\n\n- keywords :: ${keywords}\n\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
             :unnarrowed t)))))

(use-package! org-download
  :after org
  :config
  (setq-default org-download-image-dir "./images/"
                org-download-method 'directory
                org-download-heading-lvl 1
                org-image-actual-width 300
                org-download-screenshot-file "/tmp/screenshot.png"))

;; ─── 13. Chinese Support ──────────────────────────────────────
(use-package! pangu-spacing
  :config
  (global-pangu-spacing-mode 1))

(use-package! cnfonts
  :config
  ;; Font size adjustment via cnfonts UI only; no global C--/C-+ binding
  )

(when (modulep! :input chinese)
  (after! pyim
    (if (display-graphic-p)
        (setq pyim-page-tooltip 'posframe)
      (setq pyim-page-tooltip 'popup))
    (setq pyim-isearch-mode 1
          default-input-method "pyim"
          pyim-default-scheme 'quanpin
          pyim-english-input-switch-functions '(pyim-probe-isearch-mode)
          pyim-page-length 6)))

;; ─── 14. Editing Tools ────────────────────────────────────────
(use-package! smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package! visual-regexp-steroids
  :after visual-regexp)

(use-package! avy
  :config
  (setq avy-timeout-seconds 0.5))

(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package! expand-region)

(use-package! embrace)

(use-package! iedit)

(use-package! move-text)

(use-package! crux)

;; ─── 15. Platform-Specific ────────────────────────────────────
(when IS-MAC
  ;; Doom's :os macos handles command/option modifiers
  (setq mac-right-option-modifier 'none
        ns-right-option-modifier 'none)

  ;; Sync env vars from shell
  (use-package! exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

  ;; Pixel-precise smooth scrolling
  (use-package! ultra-scroll
    :config
    (ultra-scroll-mode 1)
    (setq scroll-conservatively 3
          scroll-margin 0)))

(unless IS-MAC
  ;; Linux: built-in pixel scroll
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (setq scroll-conservatively 101
        scroll-margin 0))

;; ─── 16. Clipboard & Misc ─────────────────────────────────────
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      scroll-step 2)

;; ─── 17. Load Keybindings ─────────────────────────────────────
(load! "keybindings")
