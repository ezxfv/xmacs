;;; keybindings/windows.el -*- lexical-binding: t; -*-

;; 窗口切换和管理
(map! :leader
      "0" 'treemacs-select-window
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      "8" 'split-window-below
      "9" 'split-window-right
      )

;; Zen 模式
(map!
 :desc "Zen mode"               :n "mz" #'+zen/toggle-fullscreen
 ) 