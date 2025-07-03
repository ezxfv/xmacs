;;; modules/x/ui-theme/utils.el -*- lexical-binding: t; -*-

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

(provide 'ui-theme-utils) 