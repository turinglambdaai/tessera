#lang racket/base

;; Smoke test: GLFW loads, creates a hidden GL window, reports sane sizes,
;; and tears down cleanly. Run with: raco test test/smoke-glfw.rkt

(require rackunit
         tessera/ffi/glfw)

(check-equal? (glfwInit) GLFW_TRUE "glfwInit should succeed")
(define version (glfwGetVersionString))
(check-true (string? version))
(printf "GLFW ~a\n" version)
(glfwSetErrorCallback (λ (code desc) (eprintf "GLFW error ~a: ~a\n" code desc)))

;; Hidden window: no visible flash, but a real GL context (macOS requires
;; a window for a context even for offscreen rendering).
(glfwDefaultWindowHints)
(glfwWindowHint GLFW_VISIBLE GLFW_FALSE)
(glfwWindowHint GLFW_RESIZABLE GLFW_FALSE)
;; tessera requests the legacy-profile context everywhere (see
;; tessera/platform.rkt); leaving version/profile hints at defaults does
;; exactly that

(define win (glfwCreateWindow 320 200 "tessera-smoke" #f #f))
(check-false (eq? win #f) "hidden window creation should succeed")

(glfwMakeContextCurrent win)
(define-values (fbw fbh) (glfwGetFramebufferSize win))
(check-true (and (>= fbw 320) (>= fbh 200)) (format "framebuffer ~a x ~a" fbw fbh))
(define-values (sx sy) (glfwGetWindowContentScale win))
(printf "framebuffer ~a x ~a, content scale ~a x ~a\n" fbw fbh sx sy)

(glfwSwapInterval 1)
(glfwSwapBuffers win)
(glfwDestroyWindow win)
(glfwTerminate)

;; A second full init/teardown cycle must also work (libraries often break
;; on re-init; tessera apps and tests do this repeatedly).
(check-equal? (glfwInit) GLFW_TRUE "second glfwInit should succeed")
(glfwTerminate)
