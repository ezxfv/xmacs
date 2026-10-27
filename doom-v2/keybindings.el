;;; keybindings.el -*- lexical-binding: t; -*-
;;
;; Keep Doom's navigation/search vocabulary intact. This file adds one control
;; plane under SPC a; depth follows frequency. Evil's core motions stay intact.

(map! :leader "a" nil "e" nil)

;; ═══ Direct, per-minute actions ═══════════════════════════════

;; Send region, diagnostic at point, or current line to the project agent.
(map! :nvi "M-RET" #'agent-shell-send-dwim
      "M-o" #'ace-window
      :nv "M-j" #'move-text-down
      :nv "M-k" #'move-text-up)

(map! :leader
      :desc "Jump char" "j" #'avy-goto-char-2
      :desc "Jump line" "J" #'avy-goto-line
      :desc "Expand region" "v" #'er/expand-region
      :desc "Duplicate" "d" #'crux-duplicate-current-line-or-region)

;; ═══ Agent control plane ══════════════════════════════════════
;;
;; a s  sessions     a f  send files/context
;; a p  plan         a d  diff/review
;; a w  workspace    a x  interrupt/stop

(map! :leader
      (:prefix ("a" . "agent")
       :desc "Command center" "a" #'ai-code-menu
       :desc "Send context" "RET" #'agent-shell-send-dwim
       :desc "Ask about context" "q" #'ai-code-ask-question
       :desc "Change code" "c" #'ai-code-code-change
       :desc "Explain context" "e" #'ai-code-explain
       :desc "Review changes/PR" "r" #'ai-code-pull-or-review-diff-file
       :desc "Quick prompt" "i" #'ai-code-send-quick-prompt
       (:prefix ("s" . "session")
        :desc "Switch project agent" "s" #'eden-agent-project-buffer
        :desc "Switch any agent" "a" #'eden-agent-switch-buffer
        :desc "New agent" "n" #'agent-shell-new-shell
        :desc "New worktree agent" "w" #'agent-shell-new-worktree-shell
        :desc "Dashboard" "d" #'ai-code-session-dashboard
        :desc "Transcript" "t" #'agent-shell-open-transcript
        :desc "Checkpoint/handoff" "h" #'ai-code-agent-handoff-or-checkpoint)
       (:prefix ("w" . "workspace/window")
        :desc "Toggle agent workspace" "w" #'agent-shell-workspace-toggle
        :desc "Dashboard" "d" #'ai-code-session-dashboard
        :desc "Select window" "o" #'ace-window)
       (:prefix ("f" . "file/context")
        :desc "Send current file" "f" #'ai-code-send-current-file
        :desc "Send other file" "o" #'ai-code-send-other-file
        :desc "Send region" "r" #'ai-code-send-region
        :desc "Send to agent..." "s" #'ai-code-send-dwim-to
        :desc "Manage context" "c" #'ai-code-context-action)
       (:prefix ("p" . "plan/task")
        :desc "Plan beside agent" "v" #'eden-agent-plan-layout
        :desc "Open task/plan" "p" #'eden-agent-plan
        :desc "Checkpoint" "c" #'ai-code-session-checkpoint
        :desc "Handoff" "h" #'ai-code-agent-handoff-or-checkpoint
        :desc "Prompt history" "l" #'ai-code-open-prompt-file)
       (:prefix ("d" . "diff/review")
        :desc "Diff beside agent" "v" #'eden-agent-review-layout
        :desc "Working tree diff" "d" #'eden-agent-diff
        :desc "Magit status" "s" #'magit-status
        :desc "Explain changes" "e" #'ai-code-explain-code-change
        :desc "AI review" "r" #'ai-code-pull-or-review-diff-file)
       (:prefix ("x" . "interrupt/stop")
        :desc "Interrupt agent" "x" #'agent-shell-interrupt
        :desc "Abort gptel" "g" #'gptel-abort)
       (:prefix ("o" . "other clients")
        :desc "Claude IDE" "c" #'claude-code-ide-start
        :desc "gptel chat" "g" #'gptel
        :desc "gptel menu" "m" #'gptel-menu)))

;; ═══ Editing and diagnostics ══════════════════════════════════

(map! :leader
      (:prefix ("c" . "code")
       :desc "Format buffer" "f" #'+format/buffer
       (:prefix ("d" . "diagnostics")
        :desc "List" "d" #'flycheck-list-errors
        :desc "Next" "n" #'flycheck-next-error
        :desc "Previous" "p" #'flycheck-previous-error
        :desc "Select checker" "s" #'flycheck-select-checker))
      (:prefix ("e" . "edit")
       :desc "Edit occurrences" "e" #'iedit-mode
       :desc "Surround" "s" #'embrace-commander
       (:prefix ("m" . "multiple cursors")
        :desc "Mark next" "n" #'mc/mark-next-like-this
        :desc "Mark previous" "p" #'mc/mark-previous-like-this
        :desc "Mark all" "a" #'mc/mark-all-like-this))
      (:prefix ("k" . "kubernetes")
       :desc "Overview" "o" #'kubernetes-overview
       :desc "Pod" "p" #'kubernetes-display-pod
       :desc "ConfigMap" "c" #'kubernetes-display-config-map
       :desc "Secret" "s" #'kubernetes-display-secret)
      (:prefix ("t" . "toggle")
       :desc "AI completion" "a" #'minuet-auto-suggestion-mode
       :desc "Input method" "i" #'toggle-input-method
       :desc "Zen mode" "z" #'+zen/toggle-fullscreen)
      :desc "M-x" "SPC" #'execute-extended-command
      "0" #'treemacs-select-window
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4)

;; ═══ Fallbacks and mode-local extras ══════════════════════════

(global-set-key (kbd "<f12>") #'smerge-vc-next-conflict)

(map! :leader
      :desc "Keybindings cheatsheet" "h SPC" #'eden-keybindings-help)

(map! "C-c a" #'ai-code-menu
      "C-c A" #'agent-shell-workspace-toggle
      [remap move-beginning-of-line] #'crux-move-beginning-of-line
      [remap kill-line] #'crux-smart-kill-line)

;; Plan/task buffers are Org files; retain only operations not already provided
;; by Doom's Org module.
(after! org
  (map! :localleader
        :map org-mode-map
        (:prefix ("p" . "properties")
         :desc "Set property" "p" #'org-set-property
         :desc "Delete property" "d" #'org-delete-property
         :desc "Tags" "t" #'org-set-tags)))

;; Markdown preview is deliberately low-frequency and mode-local.
(after! markdown-mode
  (map! :localleader
        :map markdown-mode-map
        :desc "Live preview" "p" #'eden-markdown-preview-mode))
