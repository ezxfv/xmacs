;;; modules/x/aidermacs/packages.el -*- lexical-binding: t; -*-

(package! aidermacs
  :recipe (:host github :repo "MatthewZMD/aidermacs"))

;; aidermacs 依赖
(package! vterm)
(package! transient)
(package! ediff)
(package! magit) 