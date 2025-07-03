;;; keybindings/base.el -*- lexical-binding: t; -*-

;; Evil 基础配置
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Leader key 设置
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; Evil snipe 配置
(setq evil-snipe-override-evil-repeat-keys nil)

;; 禁用默认键位
(map! :n "f" nil
      :n "m" nil)

(map! :map evil-motion-state-map "," nil)

(map! :niv      "C-s" nil
      :niv      "C-d" nil
      :niv      "C-i" nil
      :niv      "M-," nil
      :niv      "M-." nil

      :leader
      "A" nil
      "X" nil
      "/" nil)

;; 系统级快捷键
(global-set-key (kbd "<f12>") 'smerge-vc-next-conflict)
(global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "<f9>") 'sdcv-search-pointer+)

;; Mac 系统特定配置
(cond (IS-MAC
       (setq mac-command-modifier      'super
             ns-command-modifier       'super
             mac-option-modifier       'meta
             ns-option-modifier        'meta
             mac-right-option-modifier 'none
             ns-right-option-modifier  'none))) 