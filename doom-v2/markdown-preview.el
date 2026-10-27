;;; markdown-preview.el -*- lexical-binding: t; -*-

(require 'browse-url)
(require 'subr-x)

(defgroup eden-markdown-preview nil
  "Live browser preview for Markdown buffers."
  :group 'markdown)

(defcustom eden-markdown-preview-port 9010
  "Local HTTP port used by the Markdown preview."
  :type 'integer
  :group 'eden-markdown-preview)

(defcustom eden-markdown-preview-idle-delay 0.4
  "Idle seconds before changed Markdown is rendered."
  :type 'number
  :group 'eden-markdown-preview)

(defvar-local eden-markdown-preview--directory nil)
(defvar-local eden-markdown-preview--server nil)
(defvar-local eden-markdown-preview--timer nil)
(defvar-local eden-markdown-preview--render-process nil)
(defvar-local eden-markdown-preview--dirty nil)

(defun eden-markdown-preview--content-file ()
  (expand-file-name "content.html" eden-markdown-preview--directory))

(defun eden-markdown-preview--render ()
  "Render the current buffer asynchronously when it changed."
  (when (and eden-markdown-preview-mode
             eden-markdown-preview--dirty
             (not (process-live-p eden-markdown-preview--render-process)))
    (setq eden-markdown-preview--dirty nil)
    (let ((source (buffer-substring-no-properties (point-min) (point-max)))
          (output (generate-new-buffer " *markdown-preview-output*"))
          (target (eden-markdown-preview--content-file)))
      (setq eden-markdown-preview--render-process
            (make-process
             :name "markdown-preview-render"
             :buffer output
             :command '("npx" "-y" "marked")
             :connection-type 'pipe
             :noquery t
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (let ((process-buffer (process-buffer process)))
                   (if (zerop (process-exit-status process))
                       (with-current-buffer process-buffer
                         (write-region (point-min) (point-max) target nil 'silent)
                         (kill-buffer process-buffer))
                     (message "Markdown preview render failed; see %s"
                              (buffer-name process-buffer))))))))
      (process-send-string eden-markdown-preview--render-process source)
      (process-send-eof eden-markdown-preview--render-process))))

(defun eden-markdown-preview--mark-dirty (&rest _)
  (setq eden-markdown-preview--dirty t))

(defun eden-markdown-preview--start-server ()
  (let* ((root (file-name-directory (or buffer-file-name default-directory)))
         (base (expand-file-name "markdown-preview" doom-cache-dir)))
    (make-directory base t)
    (setq eden-markdown-preview--directory base
          eden-markdown-preview--server
          (make-process
           :name "markdown-preview-server"
           :buffer (get-buffer-create "*markdown-preview-server*")
           :command (list "node"
                          (expand-file-name "markdown-preview-server.mjs" doom-user-dir)
                          root
                          (expand-file-name "markdown-preview.html" doom-user-dir)
                          (eden-markdown-preview--content-file)
                          (number-to-string eden-markdown-preview-port))
           :noquery t))))

(defun eden-markdown-preview-open-browser ()
  "Open the current live preview in a browser."
  (interactive)
  (browse-url (format "http://127.0.0.1:%d" eden-markdown-preview-port)))

(defun eden-markdown-preview-stop ()
  "Stop live preview for the current Markdown buffer."
  (interactive)
  (setq eden-markdown-preview-mode nil)
  (when (timerp eden-markdown-preview--timer)
    (cancel-timer eden-markdown-preview--timer))
  (dolist (process (list eden-markdown-preview--render-process
                         eden-markdown-preview--server))
    (when (process-live-p process)
      (delete-process process)))
  (setq eden-markdown-preview--timer nil
        eden-markdown-preview--render-process nil
        eden-markdown-preview--server nil))

(define-minor-mode eden-markdown-preview-mode
  "Render this Markdown buffer live in a browser."
  :lighter " MDPreview"
  (if eden-markdown-preview-mode
      (progn
        (eden-markdown-preview--start-server)
        (add-hook 'after-change-functions #'eden-markdown-preview--mark-dirty nil t)
        (add-hook 'kill-buffer-hook #'eden-markdown-preview-stop nil t)
        (setq eden-markdown-preview--dirty t
              eden-markdown-preview--timer
              (run-with-idle-timer eden-markdown-preview-idle-delay t
                                   #'eden-markdown-preview--render))
        (eden-markdown-preview--render)
        (eden-markdown-preview-open-browser))
    (remove-hook 'after-change-functions #'eden-markdown-preview--mark-dirty t)
    (remove-hook 'kill-buffer-hook #'eden-markdown-preview-stop t)
    (eden-markdown-preview-stop)))

(provide 'markdown-preview)
