#lang info

(define collection "tessera")

(define deps '("base"))
(define build-deps '("rackunit-lib" "scribble-lib" "at-exp-lib"))

(define pkg-desc
  "GPU-accelerated cross-platform UI toolkit for Racket — one functional view tree, GLFW windowing, an OpenGL renderer, and real text. No C toolchain, no web stack.")
(define version "0.2.0")
(define pkg-authors '("turinglambdaai"))

(define scribblings '(("tessera.scrbl" ())))
(define test-omit-paths '("examples" "docs"))
