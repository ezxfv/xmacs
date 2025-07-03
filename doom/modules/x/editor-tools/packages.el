;;; modules/x/editor-tools/packages.el -*- lexical-binding: t; -*-

(package! smart-hungry-delete)
(package! move-text)
(package! visual-regexp)
(package! visual-regexp-steriods
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
(package! crux)
(package! string-inflection)
(package! thing-edit
  :recipe (:host github
           :repo "manateelazycat/thing-edit")) 