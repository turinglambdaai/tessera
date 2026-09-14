#lang racket/base

;; Render smoke test: draw a shape gallery into a hidden window and write a
;; PNG snapshot for visual inspection.
;; Run (needs a display): raco test test/smoke-render.rkt

(require rackunit
         racket/file
         ffi/unsafe
         tessera/platform
         tessera/ffi/gl
         tessera/render
         tessera/image)

(define pw (open-platform-window! #:width 480 #:height 320
                                  #:title "tessera-render-smoke"
                                  #:visible? #f))
(pw-make-current! pw)
(define-values (fbw fbh) (pw-framebuffer-size pw))
(define scale (/ fbw 480))

(define r (make-renderer))
(renderer-begin-frame! r fbw fbh scale)
(renderer-clear! r (color-hex "#F4F3EE"))

;; plain rect
(r-rect! r 20 20 90 60 (color-hex "#C15F3C"))
;; vertical gradient
(r-quad-grad! r 130 20 90 60 (color-hex "#2F6FED") (color-hex "#9EC1FF"))
;; rounded rect
(r-round! r 240 20 90 60 14 (color-hex "#4A4A4A"))
;; circle
(r-circle! r 365 50 30 (color-hex "#3E8E5A"))
;; border ring
(r-ring! r 20 110 90 60 12 3 (color-hex "#B0413E"))
;; hairline border (1px, feathered)
(r-ring! r 130 110 90 60 0 1 (color-hex "#7A7A7A"))
;; rounded ring
(r-ring! r 240 110 90 60 20 5 (color-hex "#2F6FED"))
;; thick line with round caps
(r-line! r 340 150 430 110 6 (color-hex "#4A4A4A"))
;; line again, thin
(r-line! r 340 170 430 170 2 (color-hex "#8A8A8A"))
;; scissor clipping: a tall rect clipped to a band
(r-scissor-push! r 20 200 120 40)
(r-rect! r 20 180 120 120 (color-hex "#2F6FED"))
(r-scissor-pop! r)
;; stacking with alpha
(r-round! r 170 200 100 60 10 (color (color-r (color-hex "#C15F3C")) (color-g (color-hex "#C15F3C")) (color-b (color-hex "#C15F3C")) 0.5))
(r-round! r 220 220 100 60 10 (color 0.18 0.44 0.93 0.5))
;; nested scissor (intersection)
(r-scissor-push! r 340 200 120 80)
(r-scissor-push! r 400 180 120 80)
(r-rect! r 340 180 140 140 (color-hex "#3E8E5A"))
(r-scissor-pop! r)
(r-scissor-pop! r)

(renderer-end-frame! r)
(glFinish)

;; snapshot and sanity checks
(define pixels (renderer-read-pixels r))
(define out (build-path (current-directory) "snapshots"))
(make-directory* out)
(png-write (build-path out "render-smoke.png") fbw fbh pixels)

;; background pixel (top-left) must be the cream clear color
(check-true (> (bytes-ref pixels 0) 230) "background red channel")
;; terracotta rect center (20..110 x 20..80 pt → ×2 device)
(define sample-x (* 2 60)) (define sample-y (* 2 50))
(define idx (+ (* sample-y fbw 4) (* sample-x 4)))
(check-true (> (bytes-ref pixels idx) 150) "terracotta rect present")
;; clipped rect must NOT appear below the scissor band
(define below (+ (* (* 2 280) fbw 4) (* (* 2 60) 4)))
(check-equal? (bytes-ref pixels below) 244 "outside clip stays background")

(close-platform-window! pw)
(platform-shutdown!)
