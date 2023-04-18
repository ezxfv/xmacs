;; general config
(setq-default history-length 999)
(setq-default prescient-history-length 1000)
(setq native-comp-async-report-warnings-errors nil)
;; (setq max-lisp-eval-depth 4096)

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

(after! keyfreq
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line))
  )

;; custom module config
(when (modulep! :completion company)
  (after! company
    (setq company-idle-delay 0
          company-minimum-prefix-length 2
          company-show-quick-access t
          company-insertion-triggers t
          company-dabbrev-downcase nil
          company-selection-wrap-around t)
    )
  )


(when (modulep! :checkers spell)
  (setq ispell-hunspell-dict-paths-alist
        `(("english" ,(concat doom-user-dir "vendor/words/dict-en-20221101_lo/en_US.aff"))))
  (setq ispell-local-dictionary "english")
  (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  )

(when (modulep! :term eshell)
  (use-package! esh-autosuggest
    ;;:hook (eshell-mode . esh-autosuggest-mode))
    ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
    ;; line below instead:
    :config
    (add-hook 'eshell-mode-hook #'esh-autosuggest-mode -100))
  )


(when (modulep! :tools lsp)
  (after! lsp-mode
    (setq lsp-idle-delay 0.10
          lsp-auto-configure t
          lsp-enable-imenu t
          lsp-enable-indentation t
          lsp-enable-xref t
          lsp-enable-snippet t
          lsp-enable-file-watchers t
          )
    (add-to-list 'lsp-file-watch-ignored-files '("[/\\\\]\\vendor\\'" "[/\\\\]\\third_party\\'" "[/\\\\]\\build\\'"))
    (setq lsp-enable-file-watchers nil)
    (lsp-register-custom-settings
     '(("gopls.completeUnimported" t t)
       ("gopls.staticcheck" t t))))
  )

(when (modulep! :ui treemacs)
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
          "pdfa.xmpi"
          ))

  (setq treemacs-file-ignore-globs
        '(;; LaTeX
          "*/_minted-*"
          ;; AucTeX
          "*/.auctex-auto"
          "*/_region_.log"
          "*/_region_.tex"))
  )


(when (modulep! :lang org)
  (use-package! org-download
    :after org
    :config
    (setq-default org-download-image-dir "./images/"
                  org-download-screenshot-method "flameshot gui --raw > %s"
                  ;;org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"
                  org-download-delete-image-after-download t
                  org-download-method 'directory
                  org-download-heading-lvl 1
                  org-image-actual-width 300
                  org-download-screenshot-file "/tmp/screenshot.png"
                  )
    )
  (when (modulep! :lang org +roam2)
    (use-package! websocket
      :after org-roam)

    (use-package! org-roam-ui
      :after org-roam ;; or :after org
      ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
      ;;         a hookable mode anymore, you're advised to pick something yourself
      ;;         if you don't care about startup time, use
      ;;  :hook (after-init . org-roam-ui-mode)
      :config
      (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t
            org-roam-ui-open-on-start t))
    (use-package! org-roam-bibtex
      :after (org-roam)
      :hook (org-roam-mode . org-roam-bibtex-mode)
      :config
      (setq org-roam-bibtex-preformat-keywords
            '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
      (setq orb-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               ""
               :file-name "${slug}"
               :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:

- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

               :unnarrowed t))))
    )
  )


(when (modulep! :input chinese)
  (after! pyim
    (if (display-graphic-p)
        (setq pyim-page-tooltip 'posframe)
      (setq pyim-page-tooltip 'popup)
      )
    (setq pyim-isearch-mode 1
          default-input-method "pyim"
          pyim-dicts `((:name greatdict :file ,(concat doom-user-dir "vendor/pyim/pyim-greatdict.pyim"))
                       (:name sogou :file ,(concat doom-user-dir "vendor/pyim/sogou.pyim")))
          pyim-default-scheme 'quanpin
          pyim-english-input-switch-functions '(pyim-probe-isearch-mode)
          pyim-page-length 6))
  )

(when (modulep! :tools pdf)
  (after! pdf-view
    ;; open pdfs scaled to fit page
    (setq-default pdf-view-display-size 'fit-width)
    (add-hook! 'pdf-view-mode-hook (evil-colemak-basics-mode -1))
    ;; automatically annotate highlights
    (setq pdf-annot-activate-created-annotations t
          pdf-view-resize-factor 1.1))
  )

(when (modulep! :lang latex)
  (use-package! company-math
    :after (:any org-mode TeX-mode)
    :config
    (set-company-backend! 'org-mode 'company-math-symbols-latex)
    (set-company-backend! 'TeX-mode 'company-math-symbols-latex)
    (set-company-backend! 'org-mode 'company-latex-commands)
    (set-company-backend! 'TeX-mode 'company-latex-commands)
    (setq company-tooltip-align-annotations t)
    (setq company-math-allow-latex-symbols-in-faces t))

  (use-package! math-symbol-lists
    :config
    (quail-define-package "math" "UTF-8" "Ω" t)
    (quail-define-rules ; add whatever extra rules you want to define here...
     ("\\from"    #X2190)
     ("\\to"      #X2192)
     ("\\lhd"     #X22B2)
     ("\\rhd"     #X22B3)
     ("\\unlhd"   #X22B4)
     ("\\unrhd"   #X22B5))
    (mapc (lambda (x)
            (if (cddr x)
                (quail-defrule (cadr x) (car (cddr x)))))
          (append math-symbol-list-basic math-symbol-list-extended math-symbol-list-subscripts math-symbol-list-superscripts)))

  (use-package! cdlatex
    :after (:any org-mode LaTeX-mode)
    :hook
    ((LaTeX-mode . turn-on-cdlatex)
     (org-mode . turn-on-org-cdlatex)))

  ;; sudo cp /usr/local/texlive/2020//texmf-var/fonts/conf/texlive-fontconfig.conf /etc/fonts/conf.d/09-texlive-fonts.conf
  ;; sudo fc-cache -fsv
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))

  (add-hook 'LaTeX-mode-hook 'add-my-latex-environments)
  (defun add-my-latex-environments ()
    (LaTeX-add-environments
     '("thm" LaTeX-env-label)
     '("prop" LaTeX-env-label)
     '("lem" LaTeX-env-label)
     '("cor" LaTeX-env-label)
     '("defn" LaTeX-env-label)
     '("not" LaTeX-env-label)
     '("rem" LaTeX-env-label)
     '("ex" LaTeX-env-label)
     '("align" LaTeX-env-label)
     '("notation" LaTeX-env-label)
     '("dmath" LaTeX-env-label)
     ))

  ;; Code I added to make syntax highlighting work in Auctex
  (custom-set-variables
   '(font-latex-math-environments (quote
                                   ("display" "displaymath" "equation" "eqnarray" "gather" "multline"
                                    "align" "alignat" "xalignat" "dmath")))
   '(TeX-insert-braces nil)) ;;Stops putting {} on argumentless commands to "save" whitespace

  ;; Additionally, reftex code to recognize this environment as an equation
  (setq reftex-label-alist
        '(("dmath" ?e nil nil t)))
  )

(when (modulep! :tools tree-sitter)
  (use-package! tree-sitter
    :hook (prog-mode . turn-on-tree-sitter-mode)
    :hook (tree-sitter-after-on . tree-sitter-hl-mode)
    :config
    (require 'tree-sitter-langs)
    ;; This makes every node a link to a section of code
    (setq tree-sitter-debug-jump-buttons t
          ;; and this highlights the entire sub tree in your code
          tree-sitter-debug-highlight-jump-region t)
    )
  )

(when (modulep! :x doom-meow)
  (setq meow-use-clipboard t)
  (setq doom-localleader-alt-key "C-l")
  (map! :map meow-leader-keymap
        "l" #'meow-keypad-start)
  )

;;(require 'ol)
;;(use-package! crosshairs)
;;(use-package! bookmark+)
(use-package! thing-edit)

(provide '+misc)
;;; +misc.el
