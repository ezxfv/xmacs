(when (modulep! :lang python +pylance)
  (require 'lsp)

  ;; download vsix from `https://www.vsixhub.com/vsix/41816/'
  ;; install: code install --install-extension vscode-pylance-2020.10.2_vsixhub.com.vsix --force
  (setq pylancejs (concat (getenv "HOME") "/.vscode/extensions/ms-python.vscode-pylance-2020.10.2/dist/server.bundle.js"))
  (unless (file-exists-p pylancejs)
    (setq pylancejs (concat (getenv "HOME") "/.vscode-server/extensions/ms-python.vscode-pylance-2020.10.2/dist/server.bundle.js"))
    )
  (setq lsp-pyright-server-cmd (list "node" pylancejs "--stdio"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () lsp-pyright-server-cmd)
                     (lambda ()
                       (and (cl-second lsp-pyright-server-cmd)
                            (file-exists-p (cl-second lsp-pyright-server-cmd)))))
    :major-modes '(python-mode)
    :server-id 'pyright
    :priority 11
    :multi-root t
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration (lsp-configuration-section "python"))))
    :notification-handlers (lsp-ht ("pyright/beginProgress" 'ignore)
                                   ("pyright/reportProgress" 'ignore)
                                   ("pyright/endProgress" 'ignore))))
  )


;;; buffer content operations
(defun evil-delete-line-forward ()
  :motion 1
  :keep-visual t
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (point)))
    (while (and (< beg end) (eq (char-after beg) ?\ ))
      (cl-incf beg))
    (if (< beg end)
        (evil-delete beg end)
      )
    )
  )

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun org-make-bold-math ()
  "If there's a selection -- wrap this with '\mathbf{' and '}'
   and put the point to the end.  Otherwise -- put the point
   between '\mathbf{' and '}'

   Also: when not in math mode -- enclose the thing in dollars."

  (interactive)

  (let (start end
              (delim "")
              (jump 1)
              )

    (when (not (texmathp))
      (setq delim "$")
      (setq jump 2)
      )

    (if (use-region-p)
        (progn
          (setq start (region-beginning))
          (setq end (region-end))

          (narrow-to-region start end)

          (goto-char (point-min))
          (insert (concat delim "\\mathbf{"))

          (goto-char (point-max))
          (insert (concat "}" delim))
          (widen)
          )

      (progn
        (insert (concat delim "\\mathbf{}" delim))
        (backward-char jump)
        )
      )
))


(defun org-make-blackboard-math ()
  "If there's a selection -- wrap this with '\mathbb{' and '}'
   and put the point to the end.  Otherwise -- put the point
   between '\mathbb{' and '}'

   Also: when not in math mode -- enclose the thing in dollars."

  (interactive)

  (let (start end
              (delim "")
              (jump 1)
              )

    (when (not (texmathp))
      (setq delim "$")
      (setq jump 2)
      )

    (if (use-region-p)
        (progn
          (setq start (region-beginning))
          (setq end (region-end))

          (narrow-to-region start end)

          (goto-char (point-min))
          (insert (concat delim "\\mathbb{"))

          (goto-char (point-max))
          (insert (concat "}" delim))
          (widen)
          )

      (progn
        (insert (concat delim "\\mathbb{}" delim))
        (backward-char jump)
        )
      )
))

(defun org-make-vert-math ()
  "If there's a selection -- wrap this with '\mathbb{' and '}'
   and put the point to the end.  Otherwise -- put the point
   between '\mathbb{' and '}'

   Also: when not in math mode -- enclose the thing in dollars."

  (interactive)

  (let (start end
              (delim "")
              (jump 1)
              )

    (when (not (texmathp))
      (setq delim "$")
      (setq jump 2)
      )

    (if (use-region-p)
        (progn
          (setq start (region-beginning))
          (setq end (region-end))

          (narrow-to-region start end)

          (goto-char (point-min))
          (insert (concat delim "‖"))

          (goto-char (point-max))
          (insert (concat "‖" delim))
          (widen)
          )

      (progn
        (insert (concat delim "‖‖" delim))
        (backward-char jump)
        )
      )
))

(defun hz-org-download-screenshot ()
  "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'."
  (interactive)
  (let ((tmp-file "/tmp/screenshot.png"))
    (delete-file tmp-file)
    (call-process-shell-command "flameshot gui -p /tmp/")
    ;; Because flameshot exit immediately, keep polling to check file existence
    (while (not (file-exists-p tmp-file))
      (sleep-for 2))
    (org-download-image tmp-file)))

(defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "org-capture") (window-system . x)))
    (select-frame-by-name "org-capture")
    (counsel-org-capture)
    (delete-other-windows)
    )

(provide 'xfunc)
