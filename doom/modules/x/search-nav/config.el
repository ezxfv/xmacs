;;; modules/x/search-nav/config.el -*- lexical-binding: t; -*-

(use-package! color-rg
  :config
  (map! :leader
        :desc "Search with color-rg" "s c" #'color-rg-search-input))

(when (modulep! :completion vertico)
  (use-package! consult-projectile
    :config
    (map! :leader
          :desc "Find project file" "SPC" #'consult-projectile-find-file))) 