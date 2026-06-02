;;; keybindings.el -*- lexical-binding: t; -*-
;;
;; Designed with Huffman coding principle:
;;   Tier 1 (per-minute):  M-key or SPC+1key  (cost ≤ 2)
;;   Tier 2 (per-hour):    SPC+2keys          (cost = 3)
;;   Tier 3 (per-session): SPC+2keys or M-key (cost = 3)
;;   Tier 4 (occasional):  SPC+2keys          (cost = 3-4)
;;
;; All bindings are terminal-safe (no C-; C-' C-: C-= C-> C-< C-S-*)
;;
;;   IRON RULE: Never rebind these evil core keys:
;;     f  F  t  T  (evil-find-char)
;;     ;  ,        (evil-repeat-find-char)
;;     /  ?  n  N  (evil-search)
;;     *  #        (evil-search-word)
;;     m           (evil-mark)

;; ═══ Tier 1: Extreme Frequency (cost ≤ 2) ═══════════════════

;; M-RET → send to AI (THE most frequent action in AI-native workflow)
(map! :nvi "M-RET" #'gptel-send)

;; SPC j → jump to any visible position (j = jump)
(map! :leader
      :desc "Jump char"   "j" #'avy-goto-char-2
      :desc "Jump line"   "J" #'avy-goto-line)

;; M-o → switch window (o = other, works in any mode)
(map! :nvi "M-o" #'ace-window)

;; SPC v → expand region (v = visual expand)
(map! :leader
      :desc "Expand region" "v" #'er/expand-region)

;; SPC d → duplicate line/region (d = duplicate)
(map! :leader
      :desc "Duplicate" "d" #'crux-duplicate-current-line-or-region)

;; ═══ Tier 2: High Frequency (cost = 3) ══════════════════════

;; ─── SPC a = AI (a = AI, home row) ───────────────────────────
(map! :leader
      (:prefix ("a" . "AI")
       :desc "AI code menu"         "a" #'ai-code-menu
       :desc "Send to AI"           "s" #'gptel-send
       :desc "Claude IDE start"     "c" #'claude-code-ide-start
       :desc "gptel chat"           "g" #'gptel
       :desc "Agent shell"          "S" #'agent-shell
       :desc "Claude Code shell"    "C" #'agent-shell-anthropic-start-claude-code
       :desc "Model/menu"           "m" #'gptel-menu
       :desc "Send region"          "r" #'claude-code-ide-send-region
       :desc "Send buffer"          "b" #'claude-code-ide-send-buffer
       :desc "Send errors"          "e" #'claude-code-ide-send-error-context
       :desc "Abort"                "x" #'gptel-abort
       :desc "Toggle completions"   "t" #'minuet-ai-mode))

;; ─── SPC c = code (diagnostics & formatting) ─────────────────
(map! :leader
      (:prefix ("c" . "code")
       :desc "Format buffer"     "f" #'+format/buffer
       :desc "List errors"       "x" #'flycheck-list-errors
       :desc "Next error"        "n" #'flycheck-next-error
       :desc "Previous error"    "p" #'flycheck-previous-error
       :desc "Select checker"    "s" #'flycheck-select-checker))

;; ─── SPC e = edit (multi-edit operations) ─────────────────────
(map! :leader
      (:prefix ("e" . "edit")
       :desc "iedit (edit all)"     "e" #'iedit-mode
       :desc "Mark next"            "n" #'mc/mark-next-like-this
       :desc "Mark previous"        "p" #'mc/mark-previous-like-this
       :desc "Mark all"             "a" #'mc/mark-all-like-this
       :desc "Surround"             "s" #'embrace-commander))

;; ─── SPC k = kubernetes ───────────────────────────────────────
(map! :leader
      (:prefix ("k" . "kubernetes")
       :desc "Overview"          "o" #'kubernetes-overview
       :desc "Display pod"       "p" #'kubernetes-display-pod
       :desc "Display config"    "c" #'kubernetes-display-config-map
       :desc "Display secret"    "s" #'kubernetes-display-secret))

;; ═══ Tier 3: Medium Frequency ════════════════════════════════

;; ─── Window management ────────────────────────────────────────
(map! :leader
      "0" #'treemacs-select-window
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      (:prefix ("w" . "window")
       :desc "Split horizontal" "s" #'split-window-below
       :desc "Split vertical"   "v" #'split-window-right))

;; ─── Move text (M-j/k = down/up, any mode) ───────────────────
(map! :nv "M-j" #'move-text-down
      :nv "M-k" #'move-text-up)

;; ─── Toggles ─────────────────────────────────────────────────
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Zen mode"          "z" #'+zen/toggle-fullscreen
       :desc "Input method"      "i" #'toggle-input-method))

;; ─── M-x ─────────────────────────────────────────────────────
(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command)

;; ═══ Tier 4: Low Frequency ═══════════════════════════════════

(global-set-key (kbd "<f12>") #'smerge-vc-next-conflict)

;; ═══ Insert Mode: C-c fallbacks ══════════════════════════════

(map!
 "C-c a" #'ai-code-menu
 "C-c d" #'crux-duplicate-current-line-or-region
 "C-c n" #'crux-cleanup-buffer-or-region)

;; ═══ Crux remaps (mode-agnostic, zero cost) ═════════════════

(map!
 [remap move-beginning-of-line] #'crux-move-beginning-of-line
 [remap kill-line]              #'crux-smart-kill-line)

;; ═══ Localleader: Language-Specific ══════════════════════════

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
