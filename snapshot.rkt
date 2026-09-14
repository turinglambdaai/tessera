#lang racket/base

;; Headless rendering: turn a view tree into a PNG without showing a
;; window. Powers tessera's own tests and gives apps pixel-precise
;; regression checks from plain racket code.
;;
;;   (require tessera/snapshot)
;;   (render-view->png "out.png"
;;                     #:width 480 #:height 320
;;                     (column (text "hello") (button "ok")))
;;
;; Needs a display (GLFW window) — under CI on Linux use xvfb-run.

(require racket/file
         racket/math
         racket/hash
         tessera/platform
         tessera/ffi/gl
         tessera/render
         tessera/image
         tessera/text
         tessera/theme
         tessera/view
         tessera/layout)

(provide render-view->png)

;; Render a view tree and write a PNG. #:scale multiplies the internal
;; framebuffer (2 = Retina-quality output).
(define (render-view->png path
                          view
                          #:width width
                          #:height height
                          #:theme [thm theme:light]
                          #:scale [scale 1.0]
                          #:font-path [font-path #f])
  (define pw (open-platform-window! #:width width #:height height
                                    #:title "tessera-snapshot"
                                    #:visible? #f))
  (dynamic-wind
    (λ () (void))
    (λ ()
      (pw-make-current! pw)
      (define-values (fbw fbh) (pw-framebuffer-size pw))
      (define renderer (make-renderer))
      (theme-current thm)
      (define font-file
        (or font-path
            (with-handlers ([exn:fail? (λ (_) #f)]) (find-font-file))))
      (unless font-file
        (error 'render-view->png "no usable TrueType font found"))
      (define cjk-file
        (with-handlers ([exn:fail? (λ (_) #f)])
          (find-font-file #:require-glyph (integer->char #x4e2d))))
      (define ctx
        (make-ui-ctx (λ (pt) (make-font-set font-file
                                            (exact-round (* pt scale))))
                     (and cjk-file
                          (λ (pt) (make-font-set cjk-file
                                                 (exact-round (* pt scale)))))
                     scale))

      (renderer-begin-frame! renderer fbw fbh scale)
      (renderer-clear! renderer (theme-bg thm))
      (define laid (layout-view ctx view width height))
      (draw-laid! renderer laid ctx)
      (renderer-end-frame! renderer)
      (glFinish)
      (define pixels (renderer-read-pixels renderer))
      (png-write path fbw fbh pixels)
      (close-platform-window! pw))
    (λ () (void))))
