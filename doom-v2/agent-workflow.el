;;; agent-workflow.el -*- lexical-binding: t; -*-

(require 'seq)

(defvar eden-agent-workspace--refresh-timer nil)
(defvar-local eden-agent-workspace--subscriptions-installed nil)

(defun eden-agent-workspace--refresh-later (&optional _event)
  "Refresh the agent sidebar once after a short quiet period."
  (when (timerp eden-agent-workspace--refresh-timer)
    (cancel-timer eden-agent-workspace--refresh-timer))
  (setq eden-agent-workspace--refresh-timer
        (run-with-idle-timer
         0.2 nil
         (lambda ()
           (setq eden-agent-workspace--refresh-timer nil)
           (when (fboundp 'agent-shell-workspace-sidebar-refresh)
             (agent-shell-workspace-sidebar-refresh))))))

(defun eden-agent-workspace-subscribe ()
  "Refresh the workspace sidebar from Agent Shell state events."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not eden-agent-workspace--subscriptions-installed))
    (setq eden-agent-workspace--subscriptions-installed t)
    (dolist (event '(init-finished input-submitted permission-request
                    permission-response turn-complete session-title-changed
                    idle error clean-up))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :event event
       :on-event #'eden-agent-workspace--refresh-later))))

(defun eden-agent-workspace-disable-polling (&rest _)
  "Disable the workspace's periodic full-buffer redraw timer."
  (when-let* ((buffer (get-buffer agent-shell-workspace-sidebar-buffer-name)))
    (with-current-buffer buffer
      (when (timerp agent-shell-workspace-sidebar--refresh-timer)
        (cancel-timer agent-shell-workspace-sidebar--refresh-timer)
        (setq agent-shell-workspace-sidebar--refresh-timer nil))))
  (dolist (buffer (agent-shell-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (eden-agent-workspace-subscribe)))))

(defun eden-agent-switch-buffer ()
  "Switch to a live Agent Shell buffer."
  (interactive)
  (require 'agent-shell)
  (let* ((buffers (seq-filter #'buffer-live-p (agent-shell-buffers)))
         (names (mapcar #'buffer-name buffers)))
    (unless names
      (user-error "No live agent buffers"))
    (pop-to-buffer (get-buffer (completing-read "Agent buffer: " names nil t)))))

(defun eden-agent--read-project-buffer ()
  "Return an Agent Shell buffer in the current project."
  (require 'agent-shell)
  (let* ((buffers (seq-filter #'buffer-live-p (agent-shell-project-buffers)))
         (names (mapcar #'buffer-name buffers)))
    (unless names
      (user-error "No agent buffers in this project"))
    (get-buffer (completing-read "Project agent: " names nil t))))

(defun eden-agent-project-buffer ()
  "Switch to an Agent Shell buffer in the current project."
  (interactive)
  (pop-to-buffer (eden-agent--read-project-buffer)))

(defun eden-agent-diff ()
  "Show the working-tree diff for the current project."
  (interactive)
  (require 'magit-diff)
  (call-interactively #'magit-diff-working-tree))

(defun eden-agent-plan ()
  "Open the current project's task/plan file."
  (interactive)
  (require 'ai-code-task)
  (call-interactively #'ai-code-create-or-open-task-file))

(defun eden-agent--side-by-side (agent companion)
  "Show AGENT on the left and COMPANION on the right."
  (delete-other-windows)
  (switch-to-buffer agent)
  (set-window-buffer (split-window-right) companion))

(defun eden-agent-plan-layout ()
  "Show a project agent beside its task/plan file."
  (interactive)
  (let ((agent (eden-agent--read-project-buffer)))
    (eden-agent-plan)
    (eden-agent--side-by-side agent (current-buffer))))

(defun eden-agent-review-layout ()
  "Show a project agent beside its working-tree diff."
  (interactive)
  (let ((agent (eden-agent--read-project-buffer)))
    (eden-agent-diff)
    (eden-agent--side-by-side agent (current-buffer))))

(provide 'agent-workflow)
