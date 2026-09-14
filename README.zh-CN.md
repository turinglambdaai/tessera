# Tessera

面向 [Racket](https://racket-lang.org/) 的 GPU 加速跨平台 UI 工具包。一套函数式视图树、GLFW 窗口、OpenGL 渲染器、真实文字渲染——你的机器上**无需任何 C 工具链**。

![Racket](https://img.shields.io/badge/Racket-9F1D20?logo=racket&logoColor=white) [![License](https://img.shields.io/badge/license-MIT-blue)](LICENSE)

[English](README.md) · **中文**

<p align="center"><img src="docs/showcase-gallery.png" alt="Tessera gallery — buttons, checkbox, progress, CJK text" width="560"></p>
<p align="center"><img src="docs/showcase-dashboard.png" alt="Tessera dashboard — cards, progress bars" width="640"></p>

## 为什么做 Tessera？

Racket 自带 `racket/gui`，能用——但很难做成现代产品级的界面，而且被绑死在单一平台观感上。Tessera 换了一条路：

- **GPU 渲染**——一条批量三角形流跑在固定管线 OpenGL 上；每帧数千个图元，稳定 60 fps
- **声明式视图**——UI 就是一个纯函数返回的不可变树；状态留在你的应用里（Elm 式 `update`）
- **真实文字**——TrueType 解析、扫线光栅化、字距、CJK 回退字体；中英混排直接可用
- **Agent 友好**——无头快照渲染（`render-view->png`），纯 `racket` 代码即可做像素级验证
- **零 C 工具链**——GLFW 与字体解析通过 Racket FFI 运行时加载；`raco pkg install` 一步到位

## 环境要求

| 依赖 | 用途 / 版本 |
|------|-------------|
| [Racket](https://racket-lang.org/) | 9.x (CS) 或更高 |
| [GLFW 3](https://www.glfw.org/) | 运行时加载，无需头文件 |

平台说明：macOS 走 legacy-profile GL 管线（见"诚实的局限"）；Linux 通过 Mesa 或厂商驱动走同一管线。

## 快速上手

### 1. 安装

```bash
git clone https://github.com/turinglambdaai/tessera.git
cd tessera
raco pkg install --name tessera --link .
```

### 2. 跑一个示例

```bash
racket examples/counter.rkt
```

### 3. 写你的应用

```racket
#lang racket/base
(require tessera)

(run #:title "计数器"
     #:width 360 #:height 220
     #:init-state 0
     #:update (λ (s m) (match m ['inc (add1 s)] ['reset 0] [else s]))
     #:view   (λ (s)
                (column #:spacing 16
                        (text (format "点击次数：~a" s) #:size 28)
                        (button "＋1" #:on-click (λ () 'inc)))))
```

模型是 Elm 式的：`state` 归你所有，`#:update` 把消息折叠进状态，`#:view` 是从状态到控件的纯函数。窗口关闭后 `run` 返回最终状态。

## 控件

| 控件 | 说明 |
|------|------|
| `text` | 样式文字：`#:size` `#:color` `#:align` |
| `button` | `#:kind` primary/secondary/ghost/danger，`#:on-click` 返回消息 |
| `checkbox` | `#:checked?` + `#:on-change` 接收新布尔值 |
| `input` | 单行文本框：`#:value` `#:on-change` `#:placeholder` `#:password?` |
| `progress` | 确定性进度条，取值 `[0,1]` |
| `divider` | 水平分割线 |
| `row` / `column` | 弹性容器：`#:spacing` `#:padding` `#:align` |
| `box` | 带内边距的圆角面板：`#:bg` `#:radius` `#:border` |
| `spacer` | 固定或 `#:flex` 弹性空白 |

关键字参数可以写在任意位置——`(column #:spacing 12 (button "ok"))` 与 `(column (button "ok") #:spacing 12)` 等价。

## 文字

拉丁文与 CJK 从 TrueType 字体（`.ttf`/`.ttc`）渲染，按平台自动解析，支持 `TESSERA_FONT` 覆盖：

```racket
(text "Racket 你好" #:size 16)          ; 中英混排
(wrap-text fs "很长的段落……" 400)         ; 贪心换行，支持 CJK 断行
```

逐字符回退：拉丁主字体 + CJK 回退字体（macOS 为 STHeiti，Linux 为 Noto CJK）覆盖混排字符串。

## 内置验证

视图即数据，可以无头渲染并对像素做断言：

```racket
(require tessera/snapshot)
(render-view->png "out.png" my-view #:width 480 #:height 320)
```

Tessera 自己的测试套件就建立在这上面：本 README 的每张截图都由 `raco test` 生成。

## 架构

```
你的应用 ──> run（Elm 循环）──> 视图树（纯数据）
                                │ 布局：测量 + 排列（点）
                                ▼
                          laid 树 ──draw-laid!──> OpenGL（GLFW 窗口）
                                                   └ 快照：PNG
```

- `tessera/platform` —— GLFW 窗口 + GL 上下文，每个 OS 一套 API
- `tessera/render` —— 批量三角形流、CPU 细分圆角几何、MSAA
- `tessera/text` —— TrueType 解析、扫线光栅化、字形图集
- `tessera/view` / `tessera/layout` —— 声明式视图与其矩形

## 诚实的局限

- **暂不支持多窗口** —— 每次 `run` 一个窗口；开第二个会关掉第一个
- **字距（kerning）已解析但未启用** —— legacy `kern` 头部有两种变体，验证后的实现将在 0.2 发布
- **拒绝 CFF/PostScript 轮廓** —— `.otf` 字体加载即失败；系统回退解析会跳过它们
- **无 IME 组合输入** —— GLFW 只投递已提交文本；CJK 输入依赖系统剪贴板/输入法工具链，不支持窗口内组合
- **`input` 仅单行** —— 多行编辑器尚未实现

## 示例

| 示例 | 演示内容 |
|------|----------|
| `examples/counter.rkt` | 最小 Elm 式应用 |
| `examples/gallery.rkt` | 单窗口展示全部控件 |
| `examples/login.rkt` | 受控输入、校验、提交流程 |
| `examples/dashboard.rkt` | 卡片、进度条、次级按钮 |

## 开发

```bash
raco test test/                 # 单元 + 冒烟 + 快照测试（需要显示器）
raco make main.rkt              # 编译
raco scribble --dest doc tessera.scrbl
```

快照测试会把 PNG 写入 `test/snapshots/`——失败后先看图，一张图胜过一条像素断言。

## 许可证

基于 [MIT License](LICENSE) 发布。
