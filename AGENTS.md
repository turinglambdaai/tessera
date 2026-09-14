# AGENTS.md — building Racket GUIs with tessera

> Quick reference for AI agents (and humans) building GUIs with
> **tessera**. Read this first. Intent-first: given what the user wants,
> it tells you which widget to use and gives a verified snippet.

## What tessera is

A GPU-accelerated UI toolkit: your UI is a pure function from state to a
view tree; the runtime draws it and routes events back as messages.

```racket
#lang racket/base
(require tessera)

(run #:title "App"
     #:init-state 0
     #:update (λ (s m) ...)
     #:view   (λ (s) ...))
```

## Decision flow

1. Does a tessera widget cover the intent? → use it (table below).
2. Is it a style/layout tweak? → keyword props on the same widget.
3. Is it a new interactive control? → compose existing widgets in a `box`;
   add a real widget only if it recurs across apps (see CONTRIBUTING.md).

## Widget intents (verified snippets)

```racket
(require tessera)
```

| Intent | Snippet |
|--------|---------|
| Heading | `(text "标题" #:size 20)` |
| Muted caption | `(text "说明" #:size 12 #:color (theme-text-muted (theme-current)))` |
| Primary action | `(button "保存" #:on-click (λ () 'save))` |
| Secondary / ghost / danger | `(button "取消" #:kind 'secondary)` |
| Boolean toggle | `(checkbox "同意" #:checked? s #:on-change (λ (v) (list 'agree v)))` |
| Text entry (controlled) | `(input s #:on-change (λ (v) (list 'name v)))` |
| Password | `(input pass #:password? #t #:on-change (λ (v) (list 'pass v)))` |
| Progress | `(progress 0.65)` |
| Card / panel | `(box #:bg (theme-surface (theme-current)) #:radius 10 (text "内容"))` |
| Horizontal group | `(row #:spacing 12 (text "A") (text "B"))` |
| Vertical group | `(column #:spacing 8 (text "A") (text "B"))` |
| Flexible spacer | `(spacer #:flex 1)` |
| Rule | `(divider)` |

## The Elm loop (how state flows)

```racket
(run #:init-state 0
     #:update (λ (s m) (match m ['inc (add1 s)] [else s]))
     #:view   (λ (s) (column (button "＋1" #:on-click (λ () 'inc))
                             (text (format "~a" s)))))
```

- Widget callbacks RETURN messages (any value, or a list of them).
- `#:update` folds each message into state.
- `#:view` is called every frame with the current state.
- `run` returns the final state when the window closes.

## Verified snippets

Every snippet in this file is executed by `test/` before release. If you
edit this file, run `raco test test/` — snippet failures fail the suite.

## Footguns — real API traps (all verified the hard way)

Read these before writing widget code; they are the mistakes an agent will
otherwise copy:

- **Callbacks return messages, not closures.** `(button "go" #:on-click
  (λ () 'go))` — the thunk returns a message for `#:update`. Side effects
  in callbacks are a design smell; put them in `update`.
- **`input` is controlled.** The value lives in YOUR state; `on-change`
  receives the new string and you must route it back through `update`.
  Forgetting this resets the field every frame.
- **`(text 42)` fails.** Content must be a string — use `(format "~a" v)`.
- **Keywords go anywhere.** `(column #:spacing 8 kids...)` and
  `(column kids... #:spacing 8)` are identical.
- **`#f` children are filtered.** `(column a #f b)` renders `a` then `b` —
  use this for conditional UI instead of `(when ...)`.
- **`(when ...)` results must still be nodes or #f** in a children list.
- **Fonts are resolved once per `run`.** Pass `#:font-path` to override the
  platform lookup; `TESSERA_FONT` also works. The CJK fallback face is
  found automatically when present.
- **Snapshot tests need a display** (GLFW window). On headless Linux CI,
  wrap with `xvfb-run -a`.
- **`raco test` sets the working directory to the test file's folder** —
  resolve output paths relative to it.

## How to verify GUI code you write

1. `raco make main.rkt` — compile check.
2. `raco test test/` — unit + smoke + snapshot tests.
3. Inspect `test/snapshots/*.png` after layout changes — a picture beats a
   pixel assertion.

## Minimal app skeleton

```racket
#lang racket/base
(require tessera)

(define (view state)
  (column
   (text (format "count = ~a" state))
   (button "＋1" #:on-click (λ () 'inc))))

(module+ main
  (run #:title "tessera"
       #:width 360 #:height 220
       #:init-state 0
       #:update (λ (s m) (match m ['inc (add1 s)] [else s]))
       #:view view))
```

Launch code lives in `(module+ main ...)` so `raco test` can load the
module without opening a window.
