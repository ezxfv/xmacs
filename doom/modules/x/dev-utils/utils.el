;;; modules/x/dev-utils/utils.el -*- lexical-binding: t; -*-

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

(provide 'dev-utils-utils) 