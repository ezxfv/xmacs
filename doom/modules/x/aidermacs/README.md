# Aidermacs for Doom Emacs

这个模块为 Doom Emacs 提供了 [Aidermacs](https://github.com/MatthewZMD/aidermacs) 的集成支持，让你可以在 Emacs 中使用 AI 辅助编程。

## 依赖

- Python 3.12
- [aider-chat](https://github.com/paul-gauthier/aider)

## 安装

1. 确保已安装 Python 3.12
2. 安装 aider:
```bash
uv tool install --force --python python3.12 aider-chat@latest --with 'httpx[socks]'
```

3. 在你的 `init.el` 中启用模块:
```elisp
(doom! :tools
       aidermacs)
```

4. 运行 `doom sync` 安装依赖

## 使用方法

主要命令都在 `SPC a` 前缀下:

- `SPC a s` - 启动 Aider 会话
- `SPC a a` - 添加文件到会话
- `SPC a i` - 交互式添加文件
- `SPC a d` - 从会话中移除文件
- `SPC a l` - 列出已添加的文件
- `SPC a h` - 显示输出历史
- `SPC a c` - 创建临时文件

## 配置

可以在 `config.el` 中自定义以下设置:

```elisp
;; 设置自动模式的文件
(setq aidermacs-auto-mode-files
      '(".aider.prompt.org"
        ".aider.chat.md"
        ".aider.chat.history.md"
        ".aider.input.history"))

;; 设置额外参数
(setq aidermacs-extra-args '("--thinking-tokens" "16k"))
```

## 注意事项

1. 确保已正确配置 AI 提供商的 API 密钥（OpenAI、Anthropic 等）
2. 首次使用时需要选择 AI 模型
3. 建议使用 vterm 作为后端以获得最佳体验 