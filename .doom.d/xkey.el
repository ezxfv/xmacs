;; map key bindings
(defalias 'ex! 'evil-ex-define-cmd)

;; file operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(map! :n "f" nil
      :n "m" nil)

(map! :map evil-motion-state-map "," nil)
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; multiple cursors hydra
(defhydra hydra-multiple-cursors (:color blue :hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

(map! :niv      "C-s" nil
      :niv      "C-d" nil
      :niv      "C-i" nil
      :niv      "M-," nil
      :niv      "M-." nil

      :leader
      "A" nil
      "X" nil
      "/" nil)

(map! [remap swiper] #'swiper-isearch
      [remap org-capture] nil
      [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
      [remap xref-find-references] #'lsp-ui-peek-find-references)

(global-set-key (kbd "<f3>") 'hydra-multiple-cursors/body)
(global-set-key (kbd "<f5>") 'deadgrep)
(global-set-key (kbd "<M-f5>") 'deadgrep-kill-all-buffers)
(global-set-key (kbd "<f12>") 'smerge-vc-next-conflict)
(global-set-key (kbd "<f11>") '+vc/smerge-hydra/body)

(map!
 "C-'"     #'imenu-list-smart-toggle
 "C-d"     (cmd! (previous-line)
                 (kill-line)
                 (forward-line))
 "C-s"     #'+default/search-buffer

 ;; smartparen
 "C-("     #'sp-backward-slurp-sexp
 "C-)"     #'sp-forward-slurp-sexp


 ;; multiple cursors
 "C->"     #'mc/mark-next-like-this
 "C-<"     #'mc/mark-previous-like-this
 "C-c C-<" #'mc/mark-all-like-this
 "C-S-c C-S-c" #'mc/edit-lines
 "C-S-c 0" #'mc/insert-numbers
 "C-S-c 1" #'mc/insert-letters
 "C-S-c s" #'mc/mark-all-in-region
 "C-S-c S" #'mc/mark-all-in-region-regexp

 ;; prefix C-c
 "C-c a c"     #'org-mac-chrome-insert-frontmost-url
 "C-c d"       #'insert-current-date-time
 "C-c t"       #'insert-current-time
 "C-c o"       #'crux-open-with
 "C-c r"       #'vr/replace
 "C-c q"       #'vr/query-replace
 "C-c u"       #'crux-view-url
 "C-c y"       #'youdao-dictionary-search-at-point+

 "C-c C-f"     #'json-mode-beautify

 :niv      "C-e"     #'evil-end-of-line
 :niv      "C--"     #'cnfonts-decrease-fontsize
 :niv      "C-+"     #'cnfonts-increase-fontsize
 :niv      "C-="     #'er/expand-region
 )

(map!
 :desc "Go function header"     :n "g[" #'beginning-of-defun
 :desc "Go function end"        :n "g]" #'end-of-defun
 :desc "Find definition"        :n "gd" #'lsp-find-definition
 :desc "Find reference"         :n "gr" #'lsp-find-references
 :desc "Find implementation"    :n "gi" #'lsp-find-implementation
 :desc "Go back find piont"     :n "gb" #'xref-pop-marker-stack
 :desc "Delete parens"          :n "z-" #'sp-splice-sexp
 :desc "Wrap with markup"       :nv "z." #'emmet-wrap-with-markup
 :desc "Increase number"        :n "+"  #'evil-numbers/inc-at-pt
 :desc "Decrease number"        :n "-"  #'evil-numbers/dec-at-pt
 :desc "List fly check errors"  :n "fl" #'list-flycheck-errors
 :desc "Next flycheck error"    :n "fn" #'flycheck-next-error
 :desc "Previous flycheck error" :n "fp" #'flycheck-previous-error
 :desc "Format buffer"          :n "fb" #'+format/buffer
 :desc "Save buffer"            :n "fs" #'save-buffer
 :desc "Comment region"         :n "mc" #'comment-or-uncomment-region
 :desc "Delete to end"          :nv "me" #'evil-delete-line
 :desc "Mark fun"               :nv "mf" #'mark-defun
 :desc "Delete to begin"        :nv "mh" #'evil-delete-line-forward
 :desc "Goto percent of buffer" :n "mgp" #'goto-buffer-percent
 :desc "Open Jupyter"           :n "mj" #'+python/open-jupyter-repl
 :desc "Multiedit match all"    :nv "mma" #'evil-multiedit-match-all
 :desc "Pull next line"         :n "mn" #'pull-next-line
 :desc "Sort Python Imports"    :n "mo" #'+python/optimize-imports
 :desc "Select checker"         :n "ms" #'flycheck-select-checker
 :desc "Toggle vterm"           :n "mt" #'+vterm/toggle
 :desc "M-x"                    :n "mx" #'execute-extended-command
 :desc "Zen mode"               :n "mz" #'+zen/toggle-fullscreen
 :desc "Comment or Uncomment Line" :nv ";" #'evilnc-comment-or-uncomment-lines
 :desc "Exec cmd async"         :nv "mai" 'async-exec-from-input
 ;;:desc "Multiedit match all"    :n "C-c RET" #'evil-multiedit-match-all
 ;;:desc "Set Mark"             :nvi    "C-m" #'set-mark-command
 )

(map! :leader
      "/" #'+default/search-project
      :desc "Open any file" "a" #'ido-find-file
      (:prefix ("d" . "debug")
       (:prefix ("b" . "breakpoint")
        "a" #'dap-breakpoint-add
        "c" #'dap-breakpoint-condition
        "d" #'dap-breakpoint-delete
        "t" #'dap-breakpoint-toggle)
       "d" #'dap-debug
       "t" #'dap-debug-edit-template)
      (:prefix ("e" . "edit")
       "b" #'move-beginning-of-line
       "e" #'move-end-of-line)
      (:prefix ("i" . "ivy")
       "t" #'ivy-taskrunner))

(map! (:leader (:desc "open window 0" :g "0" #'treemacs-select-window)) )

(map! :leader
      "0" 'treemacs-select-window
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      "8" 'split-window-below
      "9" 'split-window-right
      )

(after! org
  (map! :localleader
        :map org-mode-map
        :desc "Eval Block" "e" 'ober-eval-block-in-repl
        (:prefix ("a" . "Anki")
         :desc "Push" "p" 'anki-editor-push-notes
         :desc "Retry" "r" 'anki-editor-retry-failure-notes
         :desc "Insert" "n" 'anki-editor-insert-note
         (:prefix ("c" . "Cloze")
          :desc "Dwim" "d" 'anki-editor-cloze-dwim
          :desc "Region" "r" 'anki-editor-cloze-region
          )
         )
        (:prefix "o"
         :desc "Tags" "t" 'org-set-tags
         :desc "Roam Bibtex" "b" 'orb-note-actions
         (:prefix ("p" . "Properties")
          :desc "Set" "s" 'org-set-property
          :desc "Delete" "d" 'org-delete-property
          :desc "Actions" "a" 'org-property-action
          )
         )
        (:prefix ("i" . "Insert")
         :desc "Link/Image" "l" 'org-insert-link
         :desc "Item" "o" 'org-toggle-item
         :desc "Citation" "c" 'org-ref-helm-insert-cite-link
         :desc "Footnote" "f" 'org-footnote-action
         :desc "Table" "t" 'org-table-create-or-convert-from-region
         :desc "Screenshot" "s" 'org-download-screenshot
         (:prefix ("b" . "Math")
          :desc "Bold" "f" 'org-make-bold-math
          :desc "Blackboard" "b" 'org-make-blackboard-math
          :desc "Vert" "v" 'org-make-vert-math
          )
         (:prefix ("h" . "Headings")
          :desc "Normal" "h" 'org-insert-heading
          :desc "Todo" "t" 'org-insert-todo-heading
          (:prefix ("s" . "Subheadings")
           :desc "Normal" "s" 'org-insert-subheading
           :desc "Todo" "t" 'org-insert-todo-subheading
           )
          )
         (:prefix ("e" . "Exports")
          :desc "Dispatch" "d" 'org-export-dispatch
          )
         )
        )
  )

(map! :localleader
      :map markdown-mode-map
      :prefix ("i" . "Insert")
      :desc "Blockquote"    "q" 'markdown-insert-blockquote
      :desc "Bold"          "b" 'markdown-insert-bold
      :desc "Code"          "c" 'markdown-insert-code
      :desc "Emphasis"      "e" 'markdown-insert-italic
      :desc "Footnote"      "f" 'markdown-insert-footnote
      :desc "Code Block"    "s" 'markdown-insert-gfm-code-block
      :desc "Image"         "i" 'markdown-insert-image
      :desc "Link"          "l" 'markdown-insert-link
      :desc "List Item"     "n" 'markdown-insert-list-item
      :desc "Pre"           "p" 'markdown-insert-pre
      (:prefix ("h" . "Headings")
       :desc "One"   "1" 'markdown-insert-atx-1
       :desc "Two"   "2" 'markdown-insert-atx-2
       :desc "Three" "3" 'markdown-insert-atx-3
       :desc "Four"  "4" 'markdown-insert-atx-4
       :desc "Five"  "5" 'markdown-insert-atx-5
       :desc "Six"   "6" 'markdown-insert-atx-6))

(cond (IS-MAC
       (setq mac-command-modifier      'super
             ns-command-modifier       'super
             mac-option-modifier       'meta
             ns-option-modifier        'meta
             mac-right-option-modifier 'none
             ns-right-option-modifier  'none)))

(map! :localleader
      "n" #'my-imenu-goto-next
      "p" #'my-imenu-goto-prev)

(provide 'xkey)
