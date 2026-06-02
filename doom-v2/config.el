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

;; ─── 3. Evil Base Config ─────────────────────────────────────
(setq native-comp-async-report-warnings-errors nil)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

(setq evil-snipe-override-evil-repeat-keys nil)

;; ─── 4. Completion ────────────────────────────────────────────
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

;; ─── 5. LSP + lsp-booster ─────────────────────────────────────
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


;; ─── 6. Go Config ─────────────────────────────────────────────
(after! go-mode
  (add-hook 'before-save-hook #'lsp-format-buffer -100 t)
  (add-hook 'before-save-hook #'lsp-organize-imports -99 t)
  (add-hook 'go-mode-hook #'lsp-deferred))

;; ─── 7. Python Config ─────────────────────────────────────────
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

;; ─── 8. TypeScript / JavaScript ───────────────────────────────
(after! typescript-mode
  (add-hook 'typescript-mode-hook #'lsp-deferred))

(after! web-mode
  (when (derived-mode-p 'web-mode)
    (when (string-match-p "\\.tsx\\'" (or (buffer-file-name) ""))
      (lsp-deferred))))

;; ─── 9. Docker / Kubernetes / Helm ───────────────────────────
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

;; ─── 10. AI: Agent Shell ─────────────────────────────────────
(use-package! agent-shell
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :init
  (when (getenv "ANTHROPIC_API_KEY")
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication
           :api-key (getenv "ANTHROPIC_API_KEY")))
    (setq agent-shell-preferred-agent-config
          (agent-shell-anthropic-make-claude-code-config))
    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables
           "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY")))))

;; ─── 11. AI: gptel ──────────────────────────────────────────
(use-package! gptel
  :commands (gptel gptel-send gptel-menu gptel-abort)
  :config
  (setq gptel-model 'claude-sonnet-4-20250514
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (getenv "ANTHROPIC_API_KEY")))
  (setq gptel-default-mode 'org-mode))

;; ─── 12. AI: ai-code-interface ───────────────────────────────
(use-package! ai-code-interface
  :commands (ai-code-menu)
  :config
  (setq ai-code-interface-default-backend "claude-code"
        ai-code-interface-terminal-type 'vterm))

;; ─── 13. AI: Claude Code IDE ─────────────────────────────────
(use-package! claude-code-ide
  :commands (claude-code-ide-start
             claude-code-ide-send-region
             claude-code-ide-send-buffer
             claude-code-ide-send-error-context)
  :config
  (setq claude-code-ide-terminal-type 'vterm))

;; ─── 14. AI: minuet (inline completions) ─────────────────────
(use-package! minuet
  :hook (prog-mode . minuet-auto-suggestion-mode)
  :config
  (setq minuet-provider 'claude
        minuet-api-key (getenv "ANTHROPIC_API_KEY"))
  (setq minuet-n-completions 3
        minuet-context-window 512))

;; ─── 15. Org-mode ────────────────────────────────────────────
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

;; ─── 16. Chinese Input ───────────────────────────────────────
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

;; ─── 17. Editing Tools ───────────────────────────────────────
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

;; ─── 18. Platform-Specific ───────────────────────────────────
(when IS-MAC
  (setq mac-right-option-modifier 'none
        ns-right-option-modifier 'none)

  (use-package! exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

  (use-package! ultra-scroll
    :config
    (ultra-scroll-mode 1)
    (setq scroll-conservatively 3
          scroll-margin 0)))

(unless IS-MAC
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (setq scroll-conservatively 101
        scroll-margin 0))

;; ─── 19. Clipboard & Misc ────────────────────────────────────
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      scroll-step 2)

;; ─── 20. Load Keybindings ────────────────────────────────────
(load! "keybindings")
