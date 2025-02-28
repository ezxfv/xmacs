;; ~/.doom.d/packages/codeium/autoload.el
;;;###autoload
(defun codeium-init ()
  "初始化 Codeium 插件。"
  (when (not (bound-and-true-p codeium--initialized))
    (setq codeium--initialized t)
    (message "Codeium 插件已初始化！")))

;;;###autoload
(defun codeium-shutdown ()
  "关闭 Codeium 插件。"
  (when (bound-and-true-p codeium--initialized)
    (setq codeium--initialized nil)
    (message "Codeium 插件已关闭！")))
