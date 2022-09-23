(defun pull-next-line()
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))


(defun ivy-posframe-display-at-frame-bottom-right (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-bottom-right-corner))

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

(defun async-sentinel (p signal)
  (when (memq (process-status p) '(exit signal))
    (message "Doing something!")
    (shell-command-sentinel p signal))
  (message (process-exit-status p))
  (when (= 0 (process-exit-status p))
    (let ((buf (process-buffer p)))
      (when (get-buffer buf)
        (display-buffer buf t)
        (sit-for 1)
        (delete-window (get-buffer-window buf))
        (kill-buffer buf))))
  )

(defun async-exec (cmd &optional buf-name display-buf)
  (unless buf-name (setq buf-name "*Async shell command*"))
  (when (get-buffer buf-name)
    (kill-buffer buf-name)
    )
  (let* ((output-buffer (generate-new-buffer buf-name))
         (proc (progn
                 (display-buffer output-buffer display-buf)
                 (async-shell-command cmd output-buffer)
                 (get-buffer-process output-buffer)
                 )))
    (if (process-live-p proc)
        (set-process-sentinel proc #'async-sentinel)
      (message "No process running.")
      )
    )
  )

(defun async-exec-from-input (cmd)
  (interactive "sCMD: ")
  (async-exec cmd))

(defun goto-buffer-percent (percent)
  (interactive "nPercent: ")
  (goto-line
   (truncate
    (* (/ (if (> percent 0) percent (+ percent 100.0)) 100.0)
       (count-lines (point-min) (point-max))))))


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
(defun my-imenu-goto--closest-dir (direction)
  (imenu--make-index-alist)

  (let ((alist imenu--index-alist)
        (minoffset (point-max))
        offset pair mark imstack destination)
    ;; Elements of alist are either ("name" . marker), or
    ;; ("submenu" ("name" . marker) ... ). The list can be
    ;; Arbitrarily nested.
    (while (or alist imstack)
      (if alist
          (progn
            (setq pair (car-safe alist)
                  alist (cdr-safe alist))
            (cond
             ((atom pair)) ;; Skip anything not a cons.

             ((imenu--subalist-p pair)
              (setq imstack   (cons alist imstack)
                    alist     (cdr pair)))

             ((number-or-marker-p (setq mark (cdr pair)))
              (when (> (setq offset (* (- mark (point)) direction)) 0)
                (when (< offset minoffset) ;; Find the closest item.
                  (setq minoffset offset
                        destination mark))))))

        (setq alist   (car imstack)
              imstack (cdr imstack))))
    (when destination
      (imenu-default-goto-function "" destination ""))))

(defun my-imenu-goto-next ()
  (interactive)
  (unless (my-imenu-goto--closest-dir 1)
    (goto-char (point-max))))

(defun my-imenu-goto-prev ()
  (interactive)
  (unless (my-imenu-goto--closest-dir -1)
    (goto-char (point-min))))

(defvar etcd-host "127.0.0.1")
(defvar etcd-buf-name "etcd")

(defun ivy-etcd-get ()
  (interactive)
  (setq keys (split-string (with-current-buffer etcd-buf-name
                             (buffer-string)) "\n"))
  (ivy-read "ETCD Key: "
            keys
            :require-match t
            :action (lambda (key)
                      (message (shell-command-to-string (format "export ETCDCTL_API=3; etcdctl --command-timeout=2s --endpoints='http://%s:3379' get --print-value-only %s" etcd-host key))))
            :caller 'ivy-etcd-get)
  )

(defun ivy-etcd-del ()
  (interactive)
  (setq keys (split-string (with-current-buffer etcd-buf-name
                             (buffer-string)) "\n"))
  (ivy-read "ETCD Key: "
            keys
            :require-match t
            :action (lambda (key)
                      (message (shell-command-to-string (format "export ETCDCTL_API=3; etcdctl --command-timeout=2s --endpoints='http://%s:3379' del %s" etcd-host key))))
            :caller 'ivy-etcd-del)
  )

(defun ivy-etcd-put ()
  (interactive)
  (setq keys (split-string (with-current-buffer etcd-buf-name
                             (buffer-string)) "\n"))
  (ivy-read "ETCD Key: "
            keys
            :require-match nil
            :action (lambda (key)
                      (setq val (read-string "Val:"))
                      (message (shell-command-to-string (format "export ETCDCTL_API=3; etcdctl --command-timeout=2s --endpoints='http://%s:3379' put %s '%s'" etcd-host key val)))
                      )
            :caller 'ivy-etcd-put)
  )

(defun etcd-refresh ()
  (interactive)
  (async-exec (format "export ETCDCTL_API=3; etcdctl --command-timeout=2s --endpoints='http://%s:3379' get --keys-only --prefix '' | awk NF" etcd-host) etcd-buf-name nil)
  )

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun read-known-hosts ()
  (setq host-lines (read-lines "~/.ssh/known_hosts"))
  (cl-loop for line in host-lines
           while line
           collect (car (split-string line " ")))
  )

(defun ivy-set-etcd-host ()
  (interactive)
  (ivy-read "ETCD Host: "
            (read-known-hosts)
            :require-match nil
            :action (lambda (host)
                      (setq etcd-host host))
            :caller 'ivy-set-etcd-host)
  )

(defvar counter 0)
(defun ivy-my-ssh ()
  "Connect to a remote host by SSH."
  (interactive)
  (ivy-read "Host: "
            (read-known-hosts)
            :require-match nil
            :preselect nil
            :action (lambda (host)
                      (setq counter (+ counter 1))
                      (let (
                            (switches (split-string-and-unquote (format "-p %s ssh -oStrictHostKeyChecking=no root@%s" dev_password host)))
                            (title (concat "SSH-" (number-to-string counter)))
                            )
                        (set-buffer (apply 'make-term title "sshpass" nil switches))
                        (term-mode)
                        (term-char-mode)
                        (switch-to-buffer (concat "*" title "*"))))
            :caller 'ivy-my-ssh)
  )

(defun my/open-terminal ()
  "Open a new terminal and rename the buffer"
  (interactive)
  (setq counter (+ counter 1))
  (setq title (concat "Terminal-" (number-to-string counter)))
  (setq buf-title (concat "*" title "*"))
  (message buf-title)
  (set-buffer (make-term title "/bin/bash"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer buf-title)
  )

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
