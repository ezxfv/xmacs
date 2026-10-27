;;; keybindings-help.el -*- lexical-binding: t; -*-

(require 'button)
(require 'subr-x)

(defconst eden-keybindings-help-sections
  '(("Start here"
     ("SPC h SPC" "Open this cheatsheet")
     ("SPC a a" "Open the complete AI command center")
     ("SPC a w w" "Toggle the multi-agent workspace")
     ("M-RET" "Send region, diagnostic, or current line to the project agent"))
    ("Agent · session"
     ("SPC a s s" "Switch agent in the current project")
     ("SPC a s a" "Switch any live agent")
     ("SPC a s n" "Start a new agent")
     ("SPC a s w" "Create a Git worktree and start an agent there")
     ("SPC a s d" "Show the session dashboard")
     ("SPC a s t" "Open the current agent transcript")
     ("SPC a s h" "Create or load a checkpoint/handoff"))
    ("Agent · workspace and windows"
     ("SPC a w w" "Toggle the dedicated Agents workspace")
     ("SPC a w d" "Show the session dashboard")
     ("SPC a w o" "Select a visible window")
     ("M-o" "Select a visible window")
     ("SPC w w" "Move to the next window")
     ("SPC w h/j/k/l" "Move to the window left/down/up/right")
     ("SPC w v" "Split the window vertically")
     ("SPC w s" "Split the window horizontally")
     ("SPC w c" "Close the current window")
     ("SPC 0..4" "Select Treemacs or window 1..4"))
    ("Agent · files and context"
     ("SPC a f f" "Send the current file")
     ("SPC a f o" "Select and send another file")
     ("SPC a f r" "Send the active region")
     ("SPC a f s" "Select an agent and send DWIM context")
     ("SPC a f c" "Add, show, copy, or clear stored context")
     ("SPC a RET" "Send DWIM context to the project agent")
     ("SPC a q" "Ask about the current context")
     ("SPC a c" "Request a code change")
     ("SPC a e" "Explain the current context"))
    ("Agent · plan and task"
     ("SPC a p v" "Show Agent on the left and Plan on the right")
     ("SPC a p p" "Open or create the project task/plan file")
     ("SPC a p c" "Request a session checkpoint")
     ("SPC a p h" "Create or load a portable agent handoff")
     ("SPC a p l" "Open prompt history"))
    ("Agent · diff and review"
     ("SPC a d v" "Show Agent on the left and working-tree diff on the right")
     ("SPC a d d" "Open the working-tree diff")
     ("SPC a d s" "Open Magit status")
     ("SPC a d e" "Ask the agent to explain changes")
     ("SPC a d r" "Review a diff, branch, PR, issue, or CI result")
     ("SPC a r" "Direct shortcut for AI review"))
    ("Agent · prompts, stop, and other clients"
     ("SPC a i" "Choose and edit a quick prompt")
     ("SPC a x x" "Interrupt the current Agent Shell turn")
     ("SPC a x g" "Abort the current gptel request")
     ("SPC a o c" "Start Claude Code IDE")
     ("SPC a o g" "Open a gptel chat")
     ("SPC a o m" "Open the gptel menu"))
    ("Agents workspace sidebar"
     ("RET" "Focus the selected agent")
     ("c" "Create an agent")
     ("a / x" "Add to / remove from tiled view")
     ("t" "Return to one focused agent")
     ("s" "Toggle quick preview while moving")
     ("R" "Rename an agent buffer")
     ("m / M" "Set / cycle session mode")
     ("C-c C-c" "Interrupt the selected agent")
     ("k / r" "Kill / restart the selected agent")
     ("g" "Refresh the sidebar"))
    ("Buffer, file, and project navigation"
     ("SPC b b" "Switch buffer in the current workspace")
     ("SPC b B" "Switch any buffer")
     ("SPC b k" "Kill the current buffer")
     ("SPC f f" "Find any file")
     ("SPC p f" "Find a file in the current project")
     ("SPC p p" "Switch project")
     ("SPC p a" "Add a project")
     ("SPC TAB n" "Create a workspace")
     ("SPC TAB ." "Switch workspace"))
    ("Text and symbol search"
     ("SPC s b" "Search the current buffer")
     ("SPC s p" "Search the current project")
     ("SPC s P" "Search another project")
     ("SPC s i" "Jump to a symbol in the current buffer")
     ("SPC s I" "Jump to a symbol across open buffers")
     ("SPC j / SPC J" "Jump to a visible character pair / line")
     ("g d" "Go to definition")
     ("g r" "Find references")
     ("C-o / C-i" "Jump backward / forward"))
    ("Git and changes"
     ("SPC g g" "Open Magit status")
     ("SPC g b" "Open Git blame")
     ("SPC g l" "Open Git log")
     ("SPC g s" "Show Git status in VC gutter")
     ("[ g / ] g" "Previous / next changed hunk")
     ("<f12>" "Go to the next merge conflict"))
    ("Editing and diagnostics"
     ("SPC d" "Duplicate line or region")
     ("SPC v" "Expand the selection")
     ("M-j / M-k" "Move line or region down / up")
     ("SPC e e" "Edit all occurrences")
     ("SPC e s" "Surround selection")
     ("SPC e m n/p/a" "Mark next / previous / all occurrences")
     ("SPC c f" "Format the current buffer")
     ("SPC c d d" "List diagnostics")
     ("SPC c d n/p" "Next / previous diagnostic")
     ("SPC c d s" "Select a checker"))
    ("Toggles and local extras"
     ("SPC t a" "Toggle inline AI completion")
     ("SPC t i" "Toggle the input method")
     ("SPC t z" "Toggle distraction-free mode")
     (", p" "Toggle live preview in a Markdown buffer")
     (", p p/d/t" "Set / delete properties or tags in an Org buffer")))
  "Sections rendered by `eden-keybindings-help'.")

(defvar eden-keybindings-help-buffer-name "*Keybindings Cheatsheet*")

(defvar eden-keybindings-help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'eden-keybindings-help-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `eden-keybindings-help-mode'.")

(define-derived-mode eden-keybindings-help-mode special-mode "Key Cheatsheet"
  "Major mode for the generated keybinding cheatsheet."
  (setq-local truncate-lines nil)
  (setq-local buffer-read-only t))

(defun eden-keybindings-help--insert-section (section)
  "Insert one cheatsheet SECTION."
  (insert (propertize (car section) 'face '(:inherit outline-2 :weight bold)) "\n")
  (dolist (entry (cdr section))
    (insert "  "
            (propertize (format "%-18s" (car entry)) 'face 'help-key-binding)
            (cadr entry)
            "\n"))
  (insert "\n"))

(defun eden-keybindings-help-refresh ()
  "Regenerate the keybinding cheatsheet."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Doom Emacs · AI-native Keybindings\n"
                        'face '(:inherit outline-1 :weight bold))
            (propertize "q close   g refresh   SPC h SPC reopen\n\n"
                        'face 'shadow))
    (mapc #'eden-keybindings-help--insert-section
          eden-keybindings-help-sections)
    (goto-char (point-min))))

;;;###autoload
(defun eden-keybindings-help ()
  "Display the complete generated keybinding cheatsheet."
  (interactive)
  (let ((buffer (get-buffer-create eden-keybindings-help-buffer-name)))
    (with-current-buffer buffer
      (eden-keybindings-help-mode)
      (eden-keybindings-help-refresh))
    (pop-to-buffer buffer)))

(provide 'keybindings-help)
