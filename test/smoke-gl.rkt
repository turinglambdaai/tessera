#lang racket/base

;; Smoke test: platform layer end to end — open a hidden window, create the
;; GL context, clear to a known color, read the pixels back, tear down.
;; Run with: raco test test/smoke-gl.rkt   (needs a display)

(require rackunit
         ffi/unsafe
         tessera/platform
         tessera/ffi/gl)

(define pw (open-platform-window! #:width 320 #:height 200
                                  #:title "tessera-smoke-gl"
                                  #:visible? #f))
(check-true (platform-window? pw))

(pw-make-current! pw)
(define-values (fbw fbh) (pw-framebuffer-size pw))
(check-true (>= fbw 320) (format "framebuffer width ~a" fbw))

;; context version sanity
(define ver (gl-str (glGetString GL_VERSION)))
(check-true (string? ver))
(eprintf "GL_VERSION ~a | framebuffer ~a x ~a | scale ~a\n"
         ver fbw fbh (pw-content-scale pw))

;; clear to terracotta and read back
(glViewport 0 0 fbw fbh)
(glClearColor 0.757 0.373 0.235 1.0)   ; #C15F3C
(glClear GL_COLOR_BUFFER_BIT)
(glFinish)

(define px (make-bytes (* fbw fbh 4)))
(glReadPixels 0 0 fbw fbh GL_RGBA GL_UNSIGNED_BYTE px)

;; center pixel should be close to the clear color (GL RGBA byte order)
(define cx (* (quotient fbw 2) 4))
(define cy (* (quotient fbh 2) 4 fbw))
(define r (bytes-ref px (+ cx cy)))
(define g (bytes-ref px (+ cx cy 1)))
(define b (bytes-ref px (+ cx cy 2)))
(check-true (and (> r 180) (< r 200)) (format "red ~a" r))
(check-true (and (> g 85) (< g 105)) (format "green ~a" g))
(check-true (and (> b 50) (< b 70)) (format "blue ~a" b))

;; second window after close must also work (fresh init/teardown cycle)
(close-platform-window! pw)
(platform-shutdown!)

(define pw2 (open-platform-window! #:width 100 #:height 100 #:visible? #f))
(check-true (platform-window? pw2))
(close-platform-window! pw2)
(platform-shutdown!)
