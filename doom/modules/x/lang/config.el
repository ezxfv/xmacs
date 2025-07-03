;;; modules/x/lang/config.el -*- lexical-binding: t; -*-

;; 加载工具函数
(load! "utils")

;; 加载语言配置
(load! "+go")
(load! "+rust")
(load! "+python")

;; LSP 配置
(when (modulep! :tools lsp)
  (after! lsp-mode
    ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
    (setq lsp-idle-delay 0.10
          lsp-auto-configure t
          lsp-enable-imenu t
          lsp-enable-indentation t
          lsp-enable-xref t
          lsp-enable-snippet t
          lsp-signature-auto-activate t
          lsp-enable-file-watchers t
          lsp-go-use-placeholders nil)
    (add-to-list 'lsp-file-watch-ignored-files '("[/\\\\]\\vendor\\'" "[/\\\\]\\third_party\\'" "[/\\\\]\\build\\'"))
    (setq lsp-enable-file-watchers nil)
    (lsp-register-custom-settings
     '(("gopls.completeUnimported" t t)
       ("gopls.staticcheck" t t)))))

;; TreeSitter 配置
(when (modulep! :tools tree-sitter)
  (use-package! tree-sitter
    :hook (prog-mode . turn-on-tree-sitter-mode)
    :hook (tree-sitter-after-on . tree-sitter-hl-mode)
    :config
    (require 'tree-sitter-langs)
    ;; This makes every node a link to a section of code
    (setq tree-sitter-debug-jump-buttons t
          ;; and this highlights the entire sub tree in your code
          tree-sitter-debug-highlight-jump-region t)))

;; DAP 调试配置
(use-package! dap-mode
  :config
  (dap-auto-configure-mode)) ;; 确保自动配置调试器

(use-package! dap-dlv-go
  :after dap-mode
  :config
  (require 'dap-dlv-go)) ;; 确保加载 dap-dlv-go

;; PlantUML 配置
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

;; TreeMacs 配置
(when (modulep! :ui treemacs)
  (global-set-key (kbd "C-c C-o") (lambda () (interactive) (lsp-treemacs-call-hierarchy t)))
  (global-set-key (kbd "C-c C-i") (lambda () (interactive) (lsp-treemacs-call-hierarchy nil)))
  ;; treemacs ignore files
  (setq treemacs-file-ignore-extensions
        '(;; LaTeX
          "aux"
          "ptc"
          "fdb_latexmk"
          "fls"
          "synctex.gz"
          "toc"
          ;; LaTeX - glossary
          "glg"
          "glo"
          "gls"
          "glsdefs"
          "ist"
          "acn"
          "acr"
          "alg"
          ;; LaTeX - pgfplots
          "mw"
          ;; LaTeX - pdfx
          "pdfa.xmpi"))

  (setq treemacs-file-ignore-globs
        '(;; LaTeX
          "*/_minted-*"
          ;; AucTeX
          "*/.auctex-auto"
          "*/_region_.log"
          "*/_region_.tex"))) 