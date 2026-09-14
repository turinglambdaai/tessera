#lang scribble/manual

@title{tessera: GPU-accelerated UI for Racket}
@author{turinglambdaai}

@defmodule[tessera]

A cross-platform, GPU-accelerated UI toolkit: one functional view tree,
GLFW windowing, an OpenGL renderer, and real text — with no C toolchain
on the user side.

@section{The Elm loop}

@racketblock[
(require tessera)

(run #:title "计数器"
     #:width 360 #:height 220
     #:init-state 0
     #:update (λ (s m) (match m ['inc (add1 s)] [else s]))
     #:view   (λ (s)
                (column #:spacing 16
                        (text (format "点击次数：~a" s) #:size 28)
                        (button "＋1" #:on-click (λ () 'inc)))))
]

@racket[run] owns the window and the loop; your app owns the state.
Widget callbacks return @emph{messages}; @racket[#:update] folds each
message into state; @racket[#:view] maps state to widgets every frame.

@section{Views are data}

Every widget constructor — @racket[text], @racket[button],
@racket[column], ... — returns a plain @racket[node] structure, so views
can be built, inspected, and rendered without a window:

@racketblock[
(require tessera/snapshot)
(render-view->png "out.png" my-view #:width 480 #:height 320)
]

@section{Widgets}

@tabular[#:sep @hspace[2]
         (list (list @racket[(text str)] "styled string")
               (list @racket[(button label)] "action, returns messages")
               (list @racket[(checkbox label)] "boolean toggle")
               (list @racket[(input value)] "single-line text field")
               (list @racket[(progress v)] "determinate bar")
               (list @racket[(row kids)] "horizontal flex group")
               (list @racket[(column kids)] "vertical flex group")
               (list @racket[(box kids)] "padded rounded surface")
               (list @racket[(divider)] "horizontal rule")
               (list @racket[(spacer)] "empty space"))]

@section{Text}

TrueType faces are parsed in pure Racket and rasterized on demand into a
glyph atlas; Latin and CJK render through a per-character fallback chain.
See @secref{tessera} for the platform notes and honest gaps.

@section{Platform notes}

@itemlist[
 @item{macOS uses the legacy-profile GL pipeline; GLFW's NSGL core-profile
       path is unreliable inside a Racket process.}
 @item{Linux uses the same pipeline through Mesa or vendor drivers.}
 @item{Snapshot tests need a display; wrap with @exec{xvfb-run -a} on
       headless Linux CI.}]
