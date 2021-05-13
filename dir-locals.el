((nil . (
  (ssh-deploy-root-local . (concat (getenv "HOME") "/work/inf-deploy/"))
  (ssh-deploy-root-remote . "/ssh:root@dev:/home/inf-deploy/")
  (ssh-deploy-async . 1)
  (ssh-deploy-async-with-threads . 1)
  (ssh-deploy-on-explicit-save . 1)
  ;;(ssh-deploy-script . (lambda() (let ((default-directory ssh-deploy-root-remote)) (shell-command "bash compile.sh"))))
)))
