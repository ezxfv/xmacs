;;; modules/x/lang/packages.el -*- lexical-binding: t; -*-

;; 这里可以定义语言相关的额外包
;; 大部分语言支持已经在主配置的:lang部分定义了 

;; Go 增强工具
(package! go-playground
  :recipe (:host github :repo "grafov/go-playground"))
(package! go-tag
  :recipe (:host github :repo "brantou/emacs-go-tag"))
(package! go-fill-struct
  :recipe (:host github :repo "s-kostyaev/go-fill-struct"))

;; Python 增强工具（用于激活Python支持）
(when (modulep! :lang python)
  (package! pytest)
  (package! python-black)                ; Black 格式化工具（Ruff 兼容）
  (package! py-isort)                    ; isort 工具（Ruff 兼容）
  ) 