;;; packages.el -*- lexical-binding: t; -*-

;; ─── AI ──────────────────────────────────────────────────────
(package! agent-shell)
(package! gptel)
(package! ai-code-interface
  :recipe (:host github :repo "tninja/ai-code-interface.el"))
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide"))
(package! minuet-ai
  :recipe (:host github :repo "milanglacier/minuet-ai.el"))

;; ─── Editing Enhancements ─────────────────────────────────────
(package! crux)
(package! move-text)
(package! avy)
(package! expand-region)
(package! iedit)
(package! embrace)
(package! ace-window)

;; ─── Docker / Kubernetes / Helm ──────────────────────────────
(package! k8s-mode
  :recipe (:host github :repo "TxGVNN/emacs-k8s-mode"))
(package! kubernetes
  :recipe (:host github :repo "chrisbarrett/kubernetes-el"))

;; ─── Org Extensions ──────────────────────────────────────────
(when (featurep! :lang org +roam2)
  (package! org-roam-ui)
  (package! org-roam-bibtex))

;; ─── macOS Specific ──────────────────────────────────────────
(when IS-MAC
  (package! exec-path-from-shell
    :recipe (:host github :repo "purcell/exec-path-from-shell"))
  (package! ultra-scroll
    :recipe (:host github :repo "jdtsmith/ultra-scroll")))
