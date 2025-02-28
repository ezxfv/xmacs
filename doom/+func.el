(defun move-to-end-of-line ()
  "Move cursor to the end of current line"
  (interactive)
  (end-of-line)
  (when (not (eolp))
    (backward-char)))

(defun my/get-tabs-in-group (group)
  "Return a list of buffer names in the specified GROUP."
  (let (buffers-in-group)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (string= (centaur-tabs-get-group-name buffer) group)
                   (buffer-file-name buffer)) ; Only include buffers associated with a file
          (push (buffer-name buffer) buffers-in-group))))
    (nreverse buffers-in-group)))

(defun my/switch-to-tab-in-group ()
  "Select a tab group, search for tabs within the group, and switch to the selected tab."
  (interactive)
  (let* ((tab-groups (centaur-tabs-get-groups))
         (selected-group (consult--read
                          tab-groups
                          :prompt "Select a tab group: "
                          :require-match t
                          :sort nil
                          :history 'my/switch-to-tab-group-history))
         (tab-names-in-group (my/get-tabs-in-group selected-group))
         (selected-tab-name (consult--read
                             tab-names-in-group
                             :prompt "Select a tab: "
                             :require-match t
                             :sort nil
                             :history 'my/switch-to-tab-history
                             :lookup (lambda (_input _candidates x _)
                                       _input))))
    (switch-to-buffer (get-buffer selected-tab-name))))

(defun ssh-config-hosts ()
  "Parse .ssh/config and return a list of host configurations."
  (let ((config-file "~/.ssh/config")
        (hosts '()))
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (while (re-search-forward "^Host[ \t]+\\(.+\\)" nil t)
          (let ((host (match-string 1)))
            (unless (string-match-p "[*?]" host)
              (push host hosts))))))
    (nreverse hosts)))

(defun ssh-connect-to-host (host)
  "Connect to the given host using ssh in a new vterm window."
  (interactive (list (completing-read "Connect to host: " (ssh-config-hosts) nil t)))
  (when host
    (let ((buffer (vterm (format "*vterm-ssh-%s*" host))))
      (with-current-buffer buffer
        (vterm-send-string (format "ssh %s" host))
        (vterm-send-return)))))

(defun vterm-kill-buffer-and-window-on-exit (process event)
  "Close the vterm window when the process is terminated."
  (when (string= event "finished\n")
    (kill-buffer-and-window)))

(add-hook 'vterm-exit-functions #'vterm-kill-buffer-and-window-on-exit)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(provide '+func)
;;; +func.el ends here
