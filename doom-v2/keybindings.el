;;; keybindings.el -*- lexical-binding: t; -*-
;;
;;   IRON RULE: Never rebind these evil core keys:
;;     f  F  t  T  (evil-find-char)
;;     ;  ,        (evil-repeat-find-char)
;;     /  ?  n  N  (evil-search)
;;     *  #        (evil-search-word)
;;     m           (evil-mark)
;;
;;   Conflicts resolved from old config:
;;     C-c d  → single binding (was double-bound)
;;     C--    → removed (was double-bound)
;;     C-+    → removed (was double-bound)
;;     ff     → removed (was overriding evil-find-char)
;;     sp     → removed (was double-bound)

;; ─── Global / System ──────────────────────────────────────────
(global-set-key (kbd "<f12>") #'smerge-vc-next-conflict)
(global-set-key (kbd "C-\\") #'toggle-input-method)
(global-set-key (kbd "<f9>") #'sdcv-search-pointer+)

;; ─── Multi-Cursor ─────────────────────────────────────────────
(map!
 "C->"     #'mc/mark-next-like-this
 "C-<"     #'mc/mark-previous-like-this
 "C-c C-<" #'mc/mark-all-like-this)

;; ─── Editing ──────────────────────────────────────────────────
(map!
 [remap move-beginning-of-line] #'crux-move-beginning-of-line
 [remap kill-line]              #'crux-smart-kill-line

 "C-c d"  #'crux-duplicate-current-line-or-region
 "C-c n"  #'crux-cleanup-buffer-or-region
 "C-c f"  #'crux-recentf-find-file

 "C-="   #'er/expand-region
 "C-:"   #'avy-goto-char
 "C-'"   #'avy-goto-char-2
 "M-g f" #'avy-goto-line
 "M-g w" #'avy-goto-word-1

 "C-;"   #'iedit-mode
 "C-,"   #'embrace-commander
 "M-o"   #'ace-window

 :nv "C-S-j" #'move-text-down
 :nv "C-S-k" #'move-text-up

 :nv "C-c r" #'vr/replace
 :nv "C-c q" #'vr/query-replace)

;; ─── Windows ──────────────────────────────────────────────────
(map! :leader
      "0" #'treemacs-select-window
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "8" #'split-window-below
      "9" #'split-window-right)

;; ─── Development ──────────────────────────────────────────────
(map! :leader
      (:prefix ("c" . "code")
       :desc "Format buffer"     "f" #'+format/buffer
       :desc "List errors"       "x" #'flycheck-list-errors
       :desc "Next error"        "n" #'flycheck-next-error
       :desc "Previous error"    "p" #'flycheck-previous-error
       :desc "Select checker"    "s" #'flycheck-select-checker)

      (:prefix ("e" . "AI")
       :desc "agent-shell"       "s" #'agent-shell
       :desc "Claude Code"       "c" #'agent-shell-anthropic-start-claude-code)

      (:prefix ("k" . "kubernetes")
       :desc "Overview"          "o" #'kubernetes-overview
       :desc "Display pod"       "p" #'kubernetes-display-pod
       :desc "Display config"    "c" #'kubernetes-display-config-map
       :desc "Display secret"    "s" #'kubernetes-display-secret)

      :desc "M-x"                "SPC" #'execute-extended-command)

;; ─── Go Mode ──────────────────────────────────────────────────
(map! :localleader
      :map go-mode-map
      (:prefix ("r" . "refactor")
       :desc "Add tags"          "a" #'go-tag-add
       :desc "Remove tags"       "r" #'go-tag-remove
       :desc "Fill struct"       "f" #'go-fill-struct)
      (:prefix ("t" . "test")
       :desc "Test function"     "f" #'go-test-current-test
       :desc "Test file"         "t" #'go-test-current-file
       :desc "Test project"      "p" #'go-test-current-project
       :desc "Test coverage"     "c" #'go-test-current-coverage))

;; ─── Python Mode ──────────────────────────────────────────────
(map! :localleader
      :map python-mode-map
      (:prefix ("t" . "test")
       :desc "All tests"         "a" #'pytest-all
       :desc "Module"            "m" #'pytest-module
       :desc "One test"          "o" #'pytest-one
       :desc "Function"          "f" #'pytest-pdb-one
       :desc "Last failed"       "l" #'pytest-last-failed))

;; ─── Org Mode ─────────────────────────────────────────────────
(after! org
  (map! :localleader
        :map org-mode-map
        (:prefix ("s" . "properties")
         :desc "Tags"            "t" #'org-set-tags
         :desc "Set property"    "s" #'org-set-property
         :desc "Delete property" "d" #'org-delete-property)
        (:prefix ("l" . "insert")
         :desc "Link"            "l" #'org-insert-link
         :desc "Screenshot"      "s" #'org-download-screenshot
         :desc "Footnote"        "f" #'org-footnote-action)
        (:prefix ("x" . "export")
         :desc "Dispatch"        "d" #'org-export-dispatch)))

;; ─── Markdown Mode ────────────────────────────────────────────
(map! :localleader
      :map markdown-mode-map
      (:prefix ("i" . "insert")
       :desc "Blockquote"        "q"  #'markdown-insert-blockquote
       :desc "Bold"              "b"  #'markdown-insert-bold
       :desc "Code"              "c"  #'markdown-insert-code
       :desc "Italic"            "e"  #'markdown-insert-italic
       :desc "Footnote"          "f"  #'markdown-insert-footnote
       :desc "Code block"        "s"  #'markdown-insert-gfm-code-block
       :desc "Image"             "i"  #'markdown-insert-image
       :desc "Link"              "l"  #'markdown-insert-link
       :desc "List item"         "n"  #'markdown-insert-list-item
       (:prefix ("h" . "heading")
        :desc "Level 1"          "1"  #'markdown-insert-atx-1
        :desc "Level 2"          "2"  #'markdown-insert-atx-2
        :desc "Level 3"          "3"  #'markdown-insert-atx-3
        :desc "Level 4"          "4"  #'markdown-insert-atx-4
        :desc "Level 5"          "5"  #'markdown-insert-atx-5
        :desc "Level 6"          "6"  #'markdown-insert-atx-6)))

;; ─── Zen Mode ─────────────────────────────────────────────────
(map! :leader
      :desc "Zen mode"           "t z" #'+zen/toggle-fullscreen)
