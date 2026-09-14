#lang racket/base

;; Theme: colors, fonts, and metrics for tessera UIs. Two built-in themes —
;; a warm paper-like light theme and a deep warm dark theme — plus helpers
;; to derive variants. All colors are (color r g b a) from tessera/render.

(require racket/match
         tessera/render)

(provide (struct-out theme)
         theme:light
         theme:dark
         theme-current
         make-theme-parameter)

;; A theme is a simple record; widgets read it through the `current-theme`
;; parameter set by `run` before each frame.
(struct theme
  (name
   bg            ; window background
   surface       ; cards, panels
   surface-raised
   text          ; primary text
   text-muted    ; secondary text
   text-faint
   accent        ; primary action
   accent-hover
   accent-press
   on-accent     ; text on accent
   success warning danger
   border        ; hairlines
   input-bg
   radius        ; default corner radius, pt
   font-size     ; base font size, pt
   font-small
   font-large)
  #:transparent)

(define (make-theme name
                    #:bg bg #:surface surface #:surface-raised raised
                    #:text text #:text-muted muted #:text-faint faint
                    #:accent accent #:accent-hover hover #:accent-press press
                    #:on-accent on-accent
                    #:success success #:warning warning #:danger danger
                    #:border border #:input-bg input-bg)
  (theme name bg surface raised text muted faint accent hover press
         on-accent success warning danger border input-bg
         10 15 13 20))

(define theme:light
  (make-theme
   "light"
   #:bg (color-hex "#F4F3EE")
   #:surface (color-hex "#FFFFFF")
   #:surface-raised (color-hex "#FFFFFF")
   #:text (color-hex "#1F1E1B")
   #:text-muted (color-hex "#6B6960")
   #:text-faint (color-hex "#9B998F")
   #:accent (color-hex "#C15F3C")
   #:accent-hover (color-hex "#A94F2F")
   #:accent-press (color-hex "#93431F")
   #:on-accent (color-hex "#FFFFFF")
   #:success (color-hex "#3E7D5A")
   #:warning (color-hex "#C99700")
   #:danger (color-hex "#B0413E")
   #:border (color-hex "#DDDAD0")
   #:input-bg (color-hex "#FBFAF7")))

(define theme:dark
  (make-theme
   "dark"
   #:bg (color-hex "#232220")
   #:surface (color-hex "#2C2B28")
   #:surface-raised (color-hex "#353430")
   #:text (color-hex "#EDEBE4")
   #:text-muted (color-hex "#A8A69C")
   #:text-faint (color-hex "#77756C")
   #:accent (color-hex "#D97B57")
   #:accent-hover (color-hex "#E08A68")
   #:accent-press (color-hex "#C96A47")
   #:on-accent (color-hex "#1A1917")
   #:success (color-hex "#6FBF8F")
   #:warning (color-hex "#E0B84F")
   #:danger (color-hex "#E07A77")
   #:border (color-hex "#42403B")
   #:input-bg (color-hex "#282724")))

;; The active theme. `run` sets this per frame from the app's #:theme.
(define theme-current (make-parameter theme:light))

(define (make-theme-parameter theme)
  (make-parameter theme))
