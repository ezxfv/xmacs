# 键盘绑定配置结构

## 概述
本目录包含了按功能分类的键盘绑定配置，将原本单一的 `+keybindings.el` 文件拆分成多个功能明确的模块。

## 文件结构

```
keybindings/
├── keybindings.el              # 主入口文件，加载所有子配置
├── base.el                    # 基础键位配置
├── editing.el                 # 编辑操作
├── navigation.el              # 导航和搜索
├── windows.el                 # 窗口管理
├── development.el             # 开发功能
├── languages.el               # 语言特定
├── applications.el            # 应用功能
├── README.md                  # 本说明文档
└── modules-keybindings.md     # 模块快捷键迁移说明
```

## 各文件详细说明

### keybindings.el - 主入口文件
- 按顺序加载所有子配置文件
- 提供完整的配置加载流程

### base.el - 基础键位配置
- **Evil 配置**: 窗口分割行为、基本键位禁用
- **Leader Key**: 设置本地leader键为 `,`
- **系统快捷键**: F9词典查询、F12合并冲突、Ctrl-\输入法切换
- **Mac兼容**: macOS系统的修饰键映射

### editing.el - 编辑操作
- **多光标编辑**: `C->`, `C-<`, `C-c C-<` 等多光标操作
- **文本操作**: 行删除、区域扩展、括号删除等
- **数字操作**: `+/-` 增减数字
- **注释**: `;` 快速注释/取消注释
- **文本替换**: `C-c r/q` 可视化替换
- **编辑工具增强**: 来自 editor-tools 模块的功能
  - 文本移动: `C-S-j/k`
  - 智能编辑: Crux 功能
  - 快速跳转: Avy 功能
  - 多光标增强: `C-c m` 前缀
- **AI 代码补全**: 来自 copilot 模块的功能
  - 补全接受: `Tab/C-Tab`
  - 补全导航: `C-n/p`

### navigation.el - 导航和搜索
- **搜索功能**: `C-s` 缓冲区搜索、`/` 项目搜索
- **文件导航**: `SPC` 项目文件、`a` 任意文件
- **代码跳转**: `gd` 定义、`gr` 引用、`gi` 实现、`gb` 返回
- **Tab管理**: `mt` Tab组切换
- **搜索导航增强**: 来自 search-nav 模块的功能
  - Color RG: `SPC s g/G`
  - Deadgrep: `SPC s d`
  - RG 搜索: `SPC s r/R`
  - Projectile 集成: `SPC p f/d/b/s`

### windows.el - 窗口管理
- **窗口切换**: `0-4` 窗口和Treemacs选择
- **窗口分割**: `8` 水平分割、`9` 垂直分割
- **专注模式**: `mz` Zen模式切换

### development.el - 开发功能
- **代码检查**: `fl/fn/fp/ms` Flycheck 错误处理、选择器
- **代码格式化**: `fb` 格式化缓冲区
- **Go 开发**: 来自 dev-utils 模块的功能
  - Go 测试: `,t f/t/p/c` (Go 模式下)
- **Kubernetes**: `SPC k o/p/c/s` 管理功能
- **LSP 集成**: `C-c C-o/i` TreeMacs 调用层次

### applications.el - 应用功能
- **AI 工具**: 来自 aidermacs 模块的功能
  - Aider: `SPC e s/a/i/d/l/h/c`
  - 交互菜单: `C-c a i`
- **中文支持**: 来自 chinese 模块的功能
  - 字体调整: `C-+/-`
- **知识管理**: 来自 knowledge 模块的功能
  - 书签: `SPC m b/m/j`
- **UI 主题**: 来自 ui-theme 模块的功能
  - 十字线: `SPC t c`
  - Ultra-scroll: `SPC t u` (Mac)
- **文件操作**: `fs` 保存文件
- **实用工具**: `mx` 执行命令

## 模块快捷键迁移

本配置已将 `/modules/x/` 目录下各模块的快捷键配置迁移到对应的功能分类文件中。详细的迁移信息请参考 `modules-keybindings.md` 文件。

### 迁移的模块
- **aidermacs**: AI 编程助手快捷键 → `applications.el`
- **search-nav**: 搜索导航功能 → `navigation.el`
- **chinese**: 中文输入和字体 → `applications.el`
- **editor-tools**: 编辑工具增强 → `editing.el`
- **dev-utils**: 开发工具 → `development.el`
- **knowledge**: 知识管理 → `applications.el`
- **ui-theme**: UI 主题功能 → `applications.el`
- **lang**: 语言服务集成 → `development.el`
- **copilot**: AI 代码补全 → `editing.el`

### 模块依赖检查
所有迁移的快捷键都使用 `(when (modulep! :x module-name))` 进行模块依赖检查，确保只有在对应模块启用时才会设置相关快捷键。

### languages.el - 语言特定
- **Org-mode**: 完整的localleader绑定
  - `e` 执行代码块
  - `s` 前缀：标签、属性、Roam等
  - `l` 前缀：链接、表格、截图等插入操作
- **Markdown**: 格式化和插入操作
  - `i` 前缀：各种Markdown元素插入

### applications.el - 应用功能
- **工具集成**: Crux工具、Mac Chrome集成
- **格式化**: JSON美化、PlantUML预览
- **字体调整**: `C-+/-` 字体大小调整

## 快捷键速查

### 基础操作
- `C-\`: 切换输入法
- `F9`: 词典查询
- `F12`: 合并冲突

### 编辑
- `C->`, `C-<`: 多光标标记
- `+/-`: 数字增减
- `;`: 注释切换
- `C-=`: 扩展选择

### 导航
- `SPC`: 项目文件
- `/`: 项目搜索
- `gd/gr/gi/gb`: 代码跳转
- `mt`: Tab组切换

### 窗口
- `0-4`: 窗口选择
- `8/9`: 窗口分割
- `mz`: Zen模式

### 开发
- `fb`: 格式化
- `fs`: 保存
- `fl/fn/fp`: 错误处理

## 自定义说明

如需修改特定类型的快捷键，请编辑对应的文件：
- 想修改搜索相关 → `navigation.el`
- 想修改编辑相关 → `editing.el`
- 想修改窗口管理 → `windows.el`
- 想添加语言特定绑定 → `languages.el`

这种分类方式让配置更易维护，也方便快速定位和修改特定功能的快捷键。