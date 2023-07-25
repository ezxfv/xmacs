;; -*- no-byte-compile: t; -*-
;;; x/english/packages.el

;; ref: https://manateelazycat.github.io/emacs/2018/12/01/emacs-study-english.html
(package! websocket)

(package! deno-bridge
  :recipe (:host github :repo "manateelazycat/deno-bridge"))

(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv"))

;; (package! company-english-helper
;;   :recipe (:host github :repo "manateelazycat/company-english-helper"))

(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name"))
;; TODO support gdcv https://github.com/konstare/gdcv
