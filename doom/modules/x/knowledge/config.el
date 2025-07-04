;;; modules/x/knowledge/config.el -*- lexical-binding: t; -*-

(when (featurep! :lang org +roam2)
  (use-package! websocket
    :after org-roam)

  (use-package! org-roam-ui
    :after org-roam
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
             :unnarrowed t)))))

(use-package! bookmark+
  :config
  (setq bmkp-last-as-first-bookmark-file "/home/edenz/.config/doom/bookmarks"
        bmkp-bookmark-file "/home/edenz/.config/doom/bookmarks")
  ;; 键位绑定已迁移到 keybindings/applications.el
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
                  org-download-screenshot-file "/tmp/screenshot.png")))

(when (modulep! :tools pdf)
  (after! pdf-view
    ;; open pdfs scaled to fit page
    (setq-default pdf-view-display-size 'fit-width)
    (add-hook! 'pdf-view-mode-hook (evil-colemak-basics-mode -1))
    ;; automatically annotate highlights
    (setq pdf-annot-activate-created-annotations t
          pdf-view-resize-factor 1.1)))

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
     '("dmath" LaTeX-env-label)))

  ;; Code I added to make syntax highlighting work in Auctex
  (custom-set-variables
   '(font-latex-math-environments (quote
                                   ("display" "displaymath" "equation" "eqnarray" "gather" "multline"
                                    "align" "alignat" "xalignat" "dmath")))
   '(TeX-insert-braces nil)) ;;Stops putting {} on argumentless commands to "save" whitespace

  ;; Additionally, reftex code to recognize this environment as an equation
  (setq reftex-label-alist
        '(("dmath" ?e nil nil t))))