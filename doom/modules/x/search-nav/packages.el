;;; modules/x/search-nav/packages.el -*- lexical-binding: t; -*-

(package! color-rg
  :recipe (:host github
           :repo "manateelazycat/color-rg"))

(when (modulep! :completion vertico)
  (package! consult-projectile
    :recipe (:host gitlab :repo "OlMon/consult-projectile" :branch "master"))) 