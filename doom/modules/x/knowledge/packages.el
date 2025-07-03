;;; modules/x/knowledge/packages.el -*- lexical-binding: t; -*-

(when (featurep! :lang org +roam2)
  (unpin! org-roam)
  (package! org-roam-ui)
  (package! org-roam-bibtex))

(package! bookmark+
  :recipe (:host github
           :repo "emacsmirror/bookmark-plus")) 