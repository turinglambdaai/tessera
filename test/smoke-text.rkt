#lang racket/base

;; Text smoke test: load a system font, rasterize Latin + CJK glyphs, draw
;; wrapped lines, snapshot to PNG.
;; Run: raco test test/smoke-text.rkt   (needs a display for the GL context)

(require rackunit
         racket/file
         tessera/platform
         tessera/ffi/gl
         tessera/render
         tessera/text
         tessera/image)

(define font-path (find-font-file))
(eprintf "font: ~a\n" font-path)
(define cjk-path (find-font-file #:require-glyph #\中))
(eprintf "cjk font: ~a\n" cjk-path)

(define pw (open-platform-window! #:width 640 #:height 360
                                  #:title "tessera-text-smoke"
                                  #:visible? #f))
(pw-make-current! pw)

;; font sets need a current GL context (the atlas is a texture)
(define fs (make-font-set font-path 32))
(define cjk-fs (make-font-set cjk-path 32))
(define-values (fbw fbh) (pw-framebuffer-size pw))
(define-values (ww wh) (pw-window-size pw))
(define scale (/ fbw (max 1 ww)))

(define r (make-renderer))
(renderer-begin-frame! r fbw fbh scale)
(renderer-clear! r (color-hex "#F4F3EE"))

;; heading
(draw-text! r fs "Racket 你好 tessera" 24 24 (color-hex "#2F2F2B"))
;; body with wrapping
(define body "The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs.")
(for ([line (in-list (wrap-text fs body 400))]
      [i (in-naturals)])
  (draw-text! r fs line 24 (+ 90 (* 34 i)) (color-hex "#5A5852")))
;; CJK face renders too
(draw-text! r cjk-fs "图形界面 · 图形界面" 24 200 (color-hex "#C15F3C"))
;; small text
(define small (make-font-set font-path 16))
(draw-text! r small "small text 12.5px equivalent" 24 250 (color-hex "#8A8A85"))

(renderer-end-frame! r)
(glFinish)

(define pixels (renderer-read-pixels r))
(define out (build-path (current-directory) "snapshots"))
(make-directory* out)
(png-write (build-path out "text-smoke.png") fbw fbh pixels)

;; assertions: dark text pixels must exist in the requested logical band.
;; Convert points to framebuffer pixels using the actual platform scale; X11
;; commonly uses 1x while Retina displays use 2x.
(define (has-ink? y0 y1)
  (define stride (* fbw 4))
  (define px-y0 (max 0 (exact-round (* scale y0))))
  (define px-y1 (min fbh (exact-round (* scale y1))))
  (define px-x0 (max 0 (exact-round (* scale 24))))
  (define px-x1 (min fbw (exact-round (* scale 500))))
  (for*/or ([y (in-range px-y0 px-y1)]
            [x (in-range px-x0 px-x1)])
    (let ([i (+ (* y stride) (* x 4))])
      (< (bytes-ref pixels i) 150))))
(check-true (has-ink? 24 60) "heading glyphs rendered")
(check-true (has-ink? 90 200) "body glyphs rendered")
(check-true (has-ink? 200 240) "cjk glyphs rendered")

;; metrics sanity
(check-true (> (text-width fs "hello") (text-width fs "hi")) "width scales with text")
(check-true (>= (length (wrap-text fs body 400)) 2) "body wraps into lines")

(close-platform-window! pw)
(platform-shutdown!)
