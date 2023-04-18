(defun move-to-end-of-line ()
  "Move cursor to the end of current line"
  (interactive)
  (end-of-line)
  (when (not (eolp))
    (backward-char)))

(provide '+func)
;;; +func.el ends here
