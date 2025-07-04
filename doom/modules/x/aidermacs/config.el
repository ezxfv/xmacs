;;; modules/x/aidermacs/config.el -*- lexical-binding: t; -*-

(use-package! aidermacs
  :commands (aidermacs-start
             aidermacs-add-file
             aidermacs-add-files-interactively
             aidermacs-drop-file
             aidermacs-list-added-files
             aidermacs-show-output-history
             aidermacs-create-session-scratchpad)
  :init
  ;; 设置基本配置
  (setq aidermacs-auto-mode-files
        '(".aider.prompt.org"
          ".aider.chat.md"
          ".aider.chat.history.md"
          ".aider.input.history"))
  
  ;; 设置额外参数
  (setq aidermacs-extra-args '("--thinking-tokens" "16k"))
  
  :config
  ;; 键位绑定已迁移到 keybindings/applications.el
  ;; 配置保留在此处
  )
