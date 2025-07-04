# 模块快捷键配置说明

本文档描述了从 `/modules/x/` 目录迁移到 `/keybindings/` 目录的所有快捷键配置。

## 快捷键分类

### 1. 应用功能 (applications.el)

#### AI 工具 (aidermacs 模块)
- `SPC e s` - 启动 Aider 会话
- `SPC e a` - 添加文件
- `SPC e i` - 交互式添加文件
- `SPC e d` - 移除文件
- `SPC e l` - 列出已添加文件
- `SPC e h` - 显示输出历史
- `SPC e c` - 创建临时文件
- `C-c a i` - Aider 交互菜单

#### 中文支持 (chinese 模块)
- `C--` - 减小字体大小
- `C-+` - 增大字体大小

#### 知识管理 (knowledge 模块)
- `SPC m b` - 打开书签菜单
- `SPC m m` - 设置书签
- `SPC m j` - 跳转到书签

#### UI 主题 (ui-theme 模块)
- `SPC t c` - 切换十字线模式
- `SPC t u` - 检查 ultra-scroll 兼容性 (仅 Mac)

### 2. 开发功能 (development.el)

#### Go 开发工具 (dev-utils 模块)
- `,t f` - 测试当前函数 (Go 模式下)
- `,t t` - 测试当前文件 (Go 模式下)
- `,t p` - 测试整个项目 (Go 模式下)
- `,t c` - 测试覆盖率 (Go 模式下)

#### Kubernetes 管理 (dev-utils 模块)
- `SPC k o` - Kubernetes 概览
- `SPC k p` - 显示 Pod
- `SPC k c` - 显示配置映射
- `SPC k s` - 显示密钥

#### LSP 和 TreeMacs 集成 (lang 模块)
- `C-c C-o` - LSP 调用层次结构 (向上)
- `C-c C-i` - LSP 调用层次结构 (向下)

#### Copilot 代码补全 (copilot 模块)
- `<tab>` - 接受 Copilot 补全
- `TAB` - 接受 Copilot 补全
- `C-TAB` - 按词接受 Copilot 补全
- `C-<tab>` - 按词接受 Copilot 补全
- `C-n` - 下一个 Copilot 补全
- `C-p` - 上一个 Copilot 补全

### 3. 编辑功能 (editing.el)

#### 文本移动 (editor-tools 模块)
- `C-S-j` - 向下移动文本
- `C-S-k` - 向上移动文本

#### 正则替换 (editor-tools 模块)
- `C-c r` - 可视化正则替换
- `C-c q` - 可视化查询替换

#### Crux 智能编辑 (editor-tools 模块)
- `C-c n` - 清理缓冲区或区域
- `C-c f` - 最近文件查找
- `C-c d` - 复制当前行或区域

#### 字符串变换 (editor-tools 模块)
- `SPC c ~` - 字符串格式循环转换

#### 多点编辑 (editor-tools 模块)
- `C-;` - iedit 模式

#### 快速跳转 (editor-tools 模块)
- `C-:` - 跳转到字符
- `C-'` - 跳转到两个字符
- `M-g f` - 跳转到行
- `M-g w` - 跳转到单词

#### 窗口管理 (editor-tools 模块)
- `M-o` - Ace 窗口切换

#### 智能选择 (editor-tools 模块)
- `C-=` - 扩展选择区域
- `C--` - 收缩选择区域

#### 括号/引号操作 (editor-tools 模块)
- `C-,` - Embrace 命令器

#### 多光标编辑增强 (editor-tools 模块)
- `C-c m e` - 编辑多行
- `C-c m a` - 标记所有相同内容
- `C-c m n` - 标记下一个相同内容
- `C-c m p` - 标记上一个相同内容
- `C-c m r` - 标记区域内所有

#### 撤销树 (editor-tools 模块)
- `C-x u` - 撤销树可视化

### 4. 搜索导航 (navigation.el)

#### Color RG 搜索 (search-nav 模块)
- `SPC s g` - Color RG 搜索符号
- `SPC s G` - Color RG 输入搜索
- `SPC s d` - Deadgrep 搜索
- `SPC s r` - RG 搜索
- `SPC s R` - RG 字面搜索

#### Consult Projectile 集成 (search-nav 模块)
- `SPC p f` - 查找项目文件
- `SPC p d` - 查找项目目录
- `SPC p b` - 切换项目缓冲区
- `SPC p s` - 搜索项目

#### Counsel Projectile 集成 (search-nav 模块)
- `SPC p F` - Counsel 查找文件
- `SPC p D` - Counsel 查找目录
- `SPC p B` - Counsel 切换缓冲区
- `SPC p A` - Counsel AG 搜索

#### Anzu 搜索计数 (search-nav 模块)
- 重新映射 `query-replace` 到 `anzu-query-replace`
- 重新映射 `query-replace-regexp` 到 `anzu-query-replace-regexp`

## 模块依赖检查

所有快捷键都使用 `(when (modulep! :x module-name))` 进行条件加载，确保只有在对应模块启用时才会设置相关快捷键。

## 注意事项

1. 所有原模块中的快捷键配置已被移除，避免重复定义
2. 快捷键按功能分类到不同的文件中，便于管理和查找
3. 保留了原有的描述信息，方便 which-key 显示
4. 使用模块检查确保只在相关模块启用时加载快捷键

## 文件结构

```
keybindings/
├── keybindings.el          # 主入口文件
├── base.el                 # 基础键位配置
├── editing.el              # 编辑操作
├── navigation.el           # 导航和搜索
├── windows.el              # 窗口管理
├── development.el          # 开发功能
├── languages.el            # 语言特定
├── applications.el         # 应用功能
├── README.md               # 原有说明文档
└── modules-keybindings.md  # 本文档
```

## 使用方法

1. 确保在 `config.el` 中加载了 keybindings 配置：
   ```elisp
   (load! "keybindings/keybindings")
   ```

2. 运行 `doom sync` 同步配置

3. 重启 Emacs 或运行 `doom/reload`

4. 使用 `SPC h k` 查看具体快捷键绑定