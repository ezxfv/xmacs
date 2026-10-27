# AstroNvim 配置

这是一套基于 AstroNvim v6 的传统高效编辑器配置，不包含 AI 插件。

## 功能

- 项目与文件：Neo-tree、project.nvim
- 搜索与切换：Snacks Picker
- Git 与 diff：Gitsigns、Diffview
- 代码能力：LSP、Treesitter、格式化、诊断、补全
- 调试：nvim-dap、nvim-dap-ui、虚拟文本
- 语言：Go、Python、TypeScript/JavaScript、Rust、Lua、Shell、JSON、YAML、Docker、Markdown

## 快捷键

完整快捷键、批量修改、多光标、Diff 和调试工作流见 [`KEYBINDINGS.md`](KEYBINDINGS.md)。

在 Neovim 中按 `SPC` 查看当前上下文可用映射，或用 `SPC fk` 搜索所有已注册快捷键。

## 维护

- `SPC pa`：更新 Lazy 插件和 Mason 工具
- `:Lazy`：插件管理
- `:Mason`：LSP、格式化器和调试适配器管理
- `:checkhealth`：健康检查
