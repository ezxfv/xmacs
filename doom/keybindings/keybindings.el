;;; keybindings/keybindings.el -*- lexical-binding: t; -*-

;; 键盘绑定配置主入口文件
;; 按功能分类加载各个键盘绑定配置

;; 基础键位配置 - Leader key 设置、Evil 配置、系统快捷键
(load! "base")

;; 编辑操作 - 多光标、文本替换、数字操作等
(load! "editing")

;; 导航和搜索 - 代码跳转、文件搜索、项目导航
(load! "navigation")

;; 窗口管理 - 窗口切换、分割、Treemacs等
(load! "windows")

;; 开发功能 - 代码检查、格式化、错误处理等
(load! "development")

;; 语言特定 - Org-mode、Markdown 等模式的 localleader 绑定
(load! "languages")

;; 应用功能 - 工具集成、格式化、字体调整等
(load! "applications") 