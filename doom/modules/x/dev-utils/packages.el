;;; modules/x/dev-utils/packages.el -*- lexical-binding: t; -*-

(package! gotest)
(package! keyfreq)
(package! exec-path-from-shell
  :recipe (:host github
           :repo "purcell/exec-path-from-shell"))

;; Kubernetes 和 YAML 增强工具
(package! kubernetes
  :recipe (:host github :repo "chrisbarrett/kubernetes-el"))
(package! k8s-mode
  :recipe (:host github :repo "TxGVNN/emacs-k8s-mode"))
(package! yaml-mode) 