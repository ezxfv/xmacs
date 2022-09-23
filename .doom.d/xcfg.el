(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-quick-access t
        company-insertion-triggers t
        company-dabbrev-downcase nil
        company-selection-wrap-around t)
  ;; (add-to-list 'company-backends '(company-capf :with company-dabbrev))
  ;; (add-to-list 'company-backends 'company-ispell)
  ;; (add-hook 'evil-normal-state-entry-hook #'company-abort)

  (when (modulep! :completion company +tabnine)
    (add-to-list 'company-backends #'company-tabnine)
    (setq company-tabnine--disable-next-transform nil)
    (advice-add #'company--transform-candidates :around (lambda (func &rest args)
                                                          (if (not company-tabnine--disable-next-transform)
                                                              (apply func args)
                                                            (setq company-tabnine--disable-next-transform nil)
                                                            (car args))))
    (advice-add #'company-tabnine :around (lambda (func &rest args)
                                            (when (eq (car args) 'candidates)
                                              (setq company-tabnine--disable-next-transform t))
                                            (apply func args)))
    )
  )

(setq-default history-length 999)
(setq-default prescient-history-length 1000)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)

(use-package! lsp-mode
  :config
  (setq lsp-idle-delay 0.10
        lsp-auto-configure t
        lsp-enable-imenu t
        lsp-enable-indentation t
        lsp-enable-xref t
        lsp-enable-snippet t
        lsp-enable-file-watchers t
        )
  )

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'"))

(setq ispell-local-dictionary "/usr/share/dict/words")

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

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

;; These MODE-local-vars-hook hooks are a Doom thing. They're executed after
;; MODE-hook, on hack-local-variables-hook. Although `lsp!` is attached to
;; python-mode-local-vars-hook, it should occur earlier than my-flycheck-setup
;; this way:
(add-hook 'python-mode-local-vars-hook (lambda ()
                                         (flycheck-mode 1)
                                         (semantic-mode 1)
                                         (when (file-directory-p "~/.pyenv/versions/inf")
                                           (setq lsp-pyright-venv-path "~/.pyenv/versions/inf")
                                           (pyenv-mode-set "inf")
                                           )
                                         (when (getenv "CONDA_PREFIX_1")
                                           (setq conda-anaconda-home (getenv "CONDA_PREFIX_1"))
                                           (setq conda-env-autoactivate-mode 1)
                                           )
                                         (setq python-shell-interpreter "ipython"
                                               python-shell-interpreter-args "-i"
                                               flycheck-python-pycompile-executable "python3"
                                               flycheck-python-pylint-executable "python3"
                                               flycheck-python-flake8-executable "python3"
                                               flycheck-python-pycompile-executable "python3"
                                               doom-modeline-env-python-executable "python3"
                                               lsp-pyls-plugins-autopep8-enabled t
                                               lsp-pyls-plugins-pycodestyle-enabled t
                                               lsp-pyls-plugins-flake8-max-line-length 120
                                               lsp-pyright-auto-import-completions t)
                                         (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
                                         (flycheck-add-next-checker 'python-pyright '(warning . python-pylint))
                                         (flycheck-select-checker 'python-pyright)))

;; (after! go-mode
;;   (dap-utils-vscode-setup-function "dap-go" "golang" "go" dap-go-debug-path "0.32.0")
;;   (setq dap-go-debug-program `("node"
;;                                ,(f-join dap-go-debug-path "dist/debugAdapter.js")))
;;   )

;; go get golang.org/x/lint/golint
;; go get honnef.co/go/tools/cmd/staticcheck
(add-hook 'go-mode-local-vars-hook (lambda ()
                                     (flycheck-mode 1)
                                     (semantic-mode 1)
                                     (setq gofmt-command "goimports")
                                     (flycheck-add-next-checker 'go-staticcheck '(info . go-golint))
                                     (flycheck-select-checker 'go-staticcheck)))
(add-hook 'before-save-hook #'gofmt-before-save)

(when (modulep! :input chinese +greatdict)
  (use-package! pyim-greatdict
    :init
    (pyim-greatdict-enable)
    )
  )

(after! pyim
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup)
    )
  (setq pyim-isearch-mode 1
        pyim-default-scheme 'quanpin
        pyim-english-input-switch-functions '(pyim-probe-isearch-mode)
        pyim-page-length 5
        ivy-re-builders-alist '((t . pyim-ivy-cregexp)))
  )

(after! leetcode
  (setq leetcode-prefer-language 'python3
        leetcode-prefer-sql 'mysql
        leetcode-save-solutions t
        leetcode-directory '~/leetcode)
  )

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:.*" :size 0.25 :vslot -4 :select t :quit nil :ttl 0 :side 'bottom)
  )


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

(after! pdf-view
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  (add-hook! 'pdf-view-mode-hook (evil-colemak-basics-mode -1))
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
  ;; faster motion
  (map!
   :map pdf-view-mode-map
   :n "g g"          #'pdf-view-first-page
   :n "G"            #'pdf-view-last-page
   :n "N"            #'pdf-view-next-page-command
   :n "E"            #'pdf-view-previous-page-command
   :n "e"            #'evil-collection-pdf-view-previous-line-or-previous-page
   :n "n"            #'evil-collection-pdf-view-next-line-or-next-page
   :localleader
   (:prefix "o"
    (:prefix "n"
     :desc "Insert" "i" 'org-noter-insert-note
     ))
   ))

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

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   org_notes (concat (getenv "HOME") "/org/notes")
   org-directory org_notes
   org-noter-notes-search-path (list org_notes)))

(use-package! org-journal
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"))

(use-package! anki-editor
  :after org
  :bind (:map org-mode-map
         ("<f12>" . anki-editor-cloze-region-auto-incr)
         ("<f11>" . anki-editor-cloze-region-dont-incr)
         ("<f10>" . anki-editor-reset-cloze-number)
         ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t
        org-my-anki-file "~/org/anki/anki.org")

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))

  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))

  (defun anki-editor-reset-cloze-nußmber (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))

  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number)))

;; Org-capture templates
(after! org-capture
  (add-to-list 'org-capture-templates
               '("a" "Anki basic"
                 entry
                 (file+headline org-my-anki-file "Dispatch Shelf")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
  (add-to-list 'org-capture-templates
               '("A" "Anki cloze"
                 entry
                 (file+headline org-my-anki-file "Dispatch Shelf")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n")))


;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed 't)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 2)

(provide 'xcfg)
