;; -*- no-byte-compile: t; -*-
;;; x/english/packages.el

;; ref: https://manateelazycat.github.io/emacs/2018/12/01/emacs-study-english.html
(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv"))

(package! company-english-helper
  :recipe (:host github :repo "manateelazycat/company-english-helper"))

(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name"))

(package! txl
  :recipe (:host github :repo "tmalsburg/txl.el"))

(package! multi-translate
  :recipe (:host github :repo "twlz0ne/multi-translate.el"))
;; TODO support gdcv https://github.com/konstare/gdcv
