;;; packages.el -*- lexical-binding: t; -*-

;; ─── AI ──────────────────────────────────────────────────────
(package! agent-shell)

;; ─── Editing Enhancements ─────────────────────────────────────
(package! crux)
(package! smart-hungry-delete)
(package! move-text)
(package! visual-regexp)
(package! visual-regexp-steroids
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
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

;; ─── Go Tooling ──────────────────────────────────────────────
(package! gotest)
(package! go-tag
  :recipe (:host github :repo "brantou/emacs-go-tag"))
(package! go-fill-struct
  :recipe (:host github :repo "s-kostyaev/go-fill-struct"))

;; ─── Python Tooling ──────────────────────────────────────────
(package! pytest)

;; ─── Chinese Support ─────────────────────────────────────────
(package! pangu-spacing)
(package! cnfonts)

;; ─── Dictionary ──────────────────────────────────────────────
(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv"))

;; ─── Org Extensions ──────────────────────────────────────────
(when (featurep! :lang org +roam2)
  (package! org-roam-ui)
  (package! org-roam-bibtex))

;; ─── Performance ─────────────────────────────────────────────
(package! gcmh)

;; ─── macOS Specific ──────────────────────────────────────────
(when IS-MAC
  (package! exec-path-from-shell
    :recipe (:host github :repo "purcell/exec-path-from-shell"))
  (package! ultra-scroll
    :recipe (:host github :repo "jdtsmith/ultra-scroll")))
