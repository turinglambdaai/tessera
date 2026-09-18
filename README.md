# Tessera

A GPU-accelerated cross-platform UI toolkit for [Racket](https://racket-lang.org/). One functional view tree, GLFW windowing, an OpenGL renderer, and real text — with **no C toolchain** on your machine.

![Racket](https://img.shields.io/badge/Racket-9F1D20?logo=racket&logoColor=white) [![License](https://img.shields.io/badge/license-MIT-blue)](LICENSE)

**English** · [中文](README.zh-CN.md)

<p align="center"><img src="docs/showcase-gallery.png" alt="Tessera gallery — buttons, checkbox, progress, CJK text" width="560"></p>
<p align="center"><img src="docs/showcase-dashboard.png" alt="Tessera dashboard — cards, progress bars" width="640"></p>

## Why Tessera?

Racket ships `racket/gui`, and it works — but it is hard to style into a modern product-grade UI, and it pins you to one platform look. Tessera takes a different approach:

- **GPU-rendered** — one batched triangle stream over fixed-function OpenGL; thousands of quads per frame at 60 fps
- **Declarative views** — your UI is a plain immutable tree returned from a function; state stays in your app (Elm-style `update`)
- **Real text** — TrueType parsing, scanline rasterization, kerning, CJK via a fallback face; mixed-script strings render correctly
- **Agent-friendly** — headless snapshot rendering (`render-view->png`) gives pixel-precise verification from plain `racket` code
- **No C toolchain** — GLFW and the font parser load at runtime through Racket's FFI; `raco pkg install` is all you need after the platform runtime libraries are present

## Requirements

| Dependency | Purpose / Version |
|------------|-------------------|
| [Racket](https://racket-lang.org/) | 9.x (CS) or later |
| [GLFW 3](https://www.glfw.org/) | runtime-loaded, no headers needed |
| OpenGL runtime | Mesa on Linux or the system OpenGL framework/driver |
| TrueType font | used by the built-in text renderer |

Platform notes: macOS uses the legacy-profile GL pipeline (see honest gaps below); Linux uses the same pipeline through Mesa or vendor drivers.

## Quick Start

### 1. Install platform runtime dependencies

Ubuntu / Debian:

```bash
sudo apt update
sudo apt install -y libglfw3 libgl1-mesa-dri libglx-mesa0 fonts-dejavu-core fonts-wqy-zenhei
```

`fonts-wqy-zenhei` is used here because it provides a TrueType collection that Tessera's current pure-Racket `glyf` parser can consume. If you want CJK text on Ubuntu, point Tessera at it:

```bash
export TESSERA_FONT=/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc
```

macOS:

```bash
brew install glfw
```

### 2. Install Tessera

```bash
git clone https://github.com/turinglambdaai/tessera.git
cd tessera
raco pkg install --auto --name tessera --link "$(pwd)"
```

Using an absolute path is intentional: Racket 9.3 rejects `--link .` when `--name` is supplied because `.` is not a valid package-source name.

### 3. Run an example

```bash
racket examples/counter.rkt
```

On a headless Linux machine, run snapshot/tests under Xvfb:

```bash
sudo apt install -y xvfb
LIBGL_ALWAYS_SOFTWARE=1 xvfb-run -a racket examples/counter.rkt
```

### 4. Write your app

```racket
#lang racket/base
(require racket/match
         tessera)

(run #:title "计数器"
     #:width 360 #:height 220
     #:init-state 0
     #:update (λ (s m) (match m ['inc (add1 s)] ['reset 0] [else s]))
     #:view   (λ (s)
                (column #:spacing 16
                        (text (format "点击次数：~a" s) #:size 28)
                        (button "＋1" #:on-click (λ () 'inc)))))
```

The model is Elm-style: `state` is yours, `#:update` folds messages into it, and `#:view` is a pure function from state to widgets. `run` returns the final state after the window closes.

## Widgets

| Widget | Description |
|--------|-------------|
| `text` | styled string: `#:size` `#:color` `#:align` |
| `button` | `#:kind` primary/secondary/ghost/danger, `#:on-click` returns messages |
| `checkbox` | `#:checked?` + `#:on-change` receives the new boolean |
| `input` | single-line text field: `#:value` `#:on-change` `#:placeholder` `#:password?` |
| `progress` | determinate bar, value in `[0,1]` |
| `slider` | drag control: `#:min` `#:max` `#:on-change` fires continuously |
| `scroll` | wheel-scrollable clipped viewport with a scrollbar thumb |
| `image` | raster image (.qoi/.bmp/.tga) with `#:width`/`#:height` |
| `spinner` | indeterminate activity indicator (animated) |
| `divider` | horizontal rule |
| `row` / `column` | flex containers: `#:spacing` `#:padding` `#:align` |
| `box` | padded rounded surface: `#:bg` `#:radius` `#:border` |
| `spacer` | rigid or `#:flex` empty space |

Keywords may appear in any position — `(column #:spacing 12 (button "ok"))` and `(column (button "ok") #:spacing 12)` are the same.

## Text

Latin and CJK render from TrueType faces (`.ttf`/`.ttc`), resolved per platform with a `TESSERA_FONT` override:

```racket
(text "Racket 你好" #:size 16)          ; mixed script
(wrap-text fs "long paragraph..." 400)  ; greedy wrap, CJK-aware breaks
```

On Ubuntu, the tested CJK setup is WenQuanYi Zen Hei (`fonts-wqy-zenhei`) selected through `TESSERA_FONT`. The current parser intentionally rejects CFF/PostScript outlines, so not every `.otf`/`.ttc` package is usable yet.

## Verification built in

Because views are data, you can render them headlessly and assert on pixels:

```racket
(require tessera/snapshot)
(render-view->png "out.png" my-view #:width 480 #:height 320)
```

Tessera's own test suite is built on this: every screenshot in this README is produced by `raco test`.

## Architecture

```
your app ──> run (Elm loop) ──> view tree (plain data)
                                │ layout: measure + arrange (points)
                                ▼
                          laid tree ──draw-laid!──> OpenGL (GLFW window)
                                                   └ snapshot: PNG
```

- `tessera/platform` — GLFW windowing + GL context, one API per OS
- `tessera/render` — batched triangle stream, tessellated rounded geometry, MSAA
- `tessera/text` — TrueType parser, scanline rasterizer, glyph atlas
- `tessera/view` / `tessera/layout` — declarative views and their rectangles

## Honest gaps

- **No multi-window yet** — one window per `run`; a second window closes the first
- **Kerning is parsed but disabled** — the legacy `kern` header has two variants; the validated pass ships in 0.2
- **CFF/PostScript outlines are rejected** — `.otf` fonts fail at load; system fallback resolution skips them
- **No IME composition** — GLFW delivers committed text only; CJK input works through your OS clipboard/IME tooling, not in-window composition
- **Single-line `input`** — no multi-line editor yet
- **Kerning off** — pairwise kerning from the legacy `kern` table lands in 0.3

## Examples

| Example | Demonstrates |
|---------|--------------|
| `examples/counter.rkt` | the minimal Elm-style app |
| `examples/gallery.rkt` | every widget in one window |
| `examples/login.rkt` | controlled inputs, validation, submit flow |
| `examples/dashboard.rkt` | cards, progress bars, secondary buttons |

## Development

```bash
raco test test/                 # unit + smoke + snapshot tests (needs a display)
raco make main.rkt              # compile
raco scribble --dest doc tessera.scrbl
```

On Linux CI/headless systems:

```bash
LIBGL_ALWAYS_SOFTWARE=1 xvfb-run -a -s "-screen 0 1280x1024x24" raco test test/
```

CI runs the complete window/OpenGL/snapshot suite on Ubuntu under Xvfb + software Mesa. GitHub's hosted macOS 26 ARM64 runner currently exposes GLFW/NSGL but no usable NSGL pixel format, so that job verifies native ARM64 installation, compilation, GLFW FFI loading, and non-window tests. Full macOS rendering should be validated on a physical or self-hosted Mac with a working display/OpenGL context.

Snapshot tests write PNGs into `test/snapshots/` — inspect them after failures; a picture beats a pixel assertion.

## License

Licensed under the [MIT License](LICENSE).
