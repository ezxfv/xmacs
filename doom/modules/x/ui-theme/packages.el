;;; modules/x/ui-theme/packages.el -*- lexical-binding: t; -*-

(package! doom-nano-modeline
  :recipe (:host github
           :repo "ronisbr/doom-nano-modeline"))

(package! crosshairs
  :recipe (:host github
           :repo "emacsmirror/crosshairs"))

;; 像素级平滑滚动包，只在 Mac 平台加载
(when IS-MAC
  (package! ultra-scroll
    :recipe (:host github
             :repo "jdtsmith/ultra-scroll"))) 