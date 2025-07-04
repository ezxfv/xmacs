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
  ;; 键位绑定
  (map! :leader
        (:prefix ("a" . "AI")
         :desc "启动 Aider 会话" "s" #'aidermacs-start
         :desc "添加文件" "a" #'aidermacs-add-file
         :desc "交互式添加文件" "i" #'aidermacs-add-files-interactively
         :desc "移除文件" "d" #'aidermacs-drop-file
         :desc "列出已添加文件" "l" #'aidermacs-list-added-files
         :desc "显示输出历史" "h" #'aidermacs-show-output-history
         :desc "创建临时文件" "c" #'aidermacs-create-session-scratchpad))) 