#lang racket/base

;; tessera's platform layer: window creation, GL context lifecycle, event
;; pump, clipboard, and cursors — one API for every supported OS, via GLFW.
;;
;; Context policy: a LEGACY-PROFILE OpenGL 2.1 context everywhere. tessera's
;; renderer needs nothing newer (batched quads, client vertex arrays, GLSL
;; 120), and legacy compatibility is the one pipeline every driver ships —
;; including macOS 26, whose core-profile path is broken in practice (VAO
;; entry points reject calls, draws fail with GL_INVALID_OPERATION, while
;; the same calls succeed on a 2.1 context). Fewer branches, same pixels.
;;
;; Everything above this layer (renderer, text, UI) is pure Racket.

(require ffi/unsafe
         racket/bool
         "ffi/glfw.rkt"
         "ffi/gl.rkt")

(provide platform-window?
         platform-window-glfw
         open-platform-window!
         close-platform-window!
         platform-init!
         platform-shutdown!
         pw-make-current!
         pw-swap!
         pw-vsync!
         pw-framebuffer-size
         pw-window-size
         pw-content-scale
         pw-set-title!
         pw-set-size!
         pw-show!
         pw-maximize!
         pw-focus!
         pw-attention!
         pw-should-close?
         pw-request-close!
         pw-is-focused?
         poll-events!
         wait-events!
         wait-events-timeout!
         post-empty-event!
         pw-mouse-pos
         pw-key
         pw-mouse-button
         pw-clipboard-get
         pw-clipboard-set!
         pw-cursor!)

;; ---- state -------------------------------------------------------------------

(struct platform-window (glfw)
  #:transparent)

(define initialized? #f)

;; ---- lifecycle ---------------------------------------------------------------

(define (platform-init!)
  (unless initialized?
    (unless (= (glfwInit) GLFW_TRUE)
      (error 'tessera "GLFW failed to initialize"))
    (set! initialized? #t)))

(define (platform-shutdown!)
  (when initialized?
    (glfwTerminate)
    (set! initialized? #f)))

(define (open-platform-window! #:width width
                               #:height height
                               #:title [title "tessera"]
                               #:visible? [visible? #t]
                               #:resizable? [resizable? #t]
                               #:min-width [min-width #f]
                               #:min-height [min-height #f]
                               #:maximize [maximize #f])
  (platform-init!)
  (define (create-window samples)
    (glfwDefaultWindowHints)
    (glfwWindowHint GLFW_VISIBLE (if visible? GLFW_TRUE GLFW_FALSE))
    (glfwWindowHint GLFW_RESIZABLE (if resizable? GLFW_TRUE GLFW_FALSE))
    ;; Legacy-profile context on every platform (see module note). Leaving the
    ;; version/profile hints at their defaults requests exactly that.
    (glfwWindowHint GLFW_CLIENT_API GLFW_OPENGL_API)
    (glfwWindowHint GLFW_SAMPLES samples)
    (glfwCreateWindow width height title #f #f))
  ;; Prefer 4x MSAA for rounded geometry, but do not make multisampling a hard
  ;; platform requirement. Headless/virtualized macOS environments in
  ;; particular may expose a valid OpenGL context without a multisample pixel
  ;; format. Falling back to zero samples keeps the app usable; GL_MULTISAMPLE
  ;; is harmless when the framebuffer has no multisample buffers.
  (define win
    (or (create-window 4)
        (begin
          (eprintf "tessera: 4x MSAA unavailable; retrying without multisampling\n")
          (create-window 0))))
  (unless win
    (error 'tessera "failed to create a window (is a display available?)"))
  (when (and min-width min-height)
    (glfwSetWindowSizeLimits win min-width min-height GLFW_DONT_CARE GLFW_DONT_CARE))
  (glfwMakeContextCurrent win)
  (install-gl-loader!)
  (glfwSwapInterval 1)
  (when maximize (glfwMaximizeWindow win))
  (platform-window win))

(define (close-platform-window! pw)
  (glfwDestroyWindow (platform-window-glfw pw)))

;; ---- GL plumbing --------------------------------------------------------------

;; Every GL entry point tessera uses is a GL 2.1-era export. Resolution is
;; platform-dispatched: on macOS, GLFW's proc lookup serves only contexts
;; created through its NSGL core path, so we dlsym OpenGL.framework directly
;; (every legacy export is a direct symbol there); elsewhere
;; glfwGetProcAddress works.
(define (install-gl-loader!)
  (if (eq? (system-type 'os) 'macosx)
      (let ([libgl (ffi-lib "/System/Library/Frameworks/OpenGL.framework/OpenGL")])
        (gl-set-loader!
         (λ (name)
           (with-handlers ([exn:fail? (λ (_) #f)])
             (get-ffi-obj name libgl _fpointer)))))
      (gl-set-loader! (λ (name) (glfwGetProcAddress name)))))

;; ---- per-frame GL --------------------------------------------------------------

(define (pw-make-current! pw)
  (glfwMakeContextCurrent (platform-window-glfw pw)))

(define (pw-swap! pw)
  (glfwSwapBuffers (platform-window-glfw pw)))

(define (pw-vsync! pw on?)
  (glfwSwapInterval (if on? 1 0)))

;; ---- queries -------------------------------------------------------------------

(define (pw-framebuffer-size pw)
  (glfwGetFramebufferSize (platform-window-glfw pw)))

(define (pw-window-size pw)
  (glfwGetWindowSize (platform-window-glfw pw)))

(define (pw-content-scale pw)
  (define-values (sx sy) (glfwGetWindowContentScale (platform-window-glfw pw)))
  (max sx sy))

(define (pw-set-title! pw title)
  (glfwSetWindowTitle (platform-window-glfw pw) title))

(define (pw-set-size! pw w h)
  (glfwSetWindowSize (platform-window-glfw pw) w h))

;; Deliberately shown late (after the first frame is rendered) so users never
;; see an empty window flash.
(define (pw-show! pw)
  (glfwShowWindow (platform-window-glfw pw)))

(define (pw-maximize! pw) (glfwMaximizeWindow (platform-window-glfw pw)))
(define (pw-focus! pw) (glfwFocusWindow (platform-window-glfw pw)))
(define (pw-attention! pw) (glfwRequestWindowAttention (platform-window-glfw pw)))

(define (pw-should-close? pw)
  (= (glfwWindowShouldClose (platform-window-glfw pw)) GLFW_TRUE))

(define (pw-request-close! pw)
  (glfwSetWindowShouldClose (platform-window-glfw pw) GLFW_TRUE))

(define (pw-is-focused? pw)
  (= (glfwGetWindowAttrib (platform-window-glfw pw) GLFW_FOCUSED) GLFW_TRUE))

;; ---- event pump -----------------------------------------------------------------

(define (poll-events!) (glfwPollEvents))
(define (wait-events!) (glfwWaitEvents))
(define (wait-events-timeout! s) (glfwWaitEventsTimeout s))
(define (post-empty-event!) (glfwPostEmptyEvent))

;; ---- input ----------------------------------------------------------------------

(define (pw-mouse-pos pw)
  (glfwGetCursorPos (platform-window-glfw pw)))

(define (pw-key pw key)
  (= (glfwGetKey (platform-window-glfw pw) key) GLFW_PRESS))

(define (pw-mouse-button pw button)
  (= (glfwGetMouseButton (platform-window-glfw pw) button) GLFW_PRESS))

;; ---- clipboard -------------------------------------------------------------------

(define (pw-clipboard-get pw)
  (define p (glfwGetClipboardString (platform-window-glfw pw)))
  (and p (ptr-ref p _string/utf-8)))

(define (pw-clipboard-set! pw s)
  (glfwSetClipboardString (platform-window-glfw pw) s))

;; ---- cursors ---------------------------------------------------------------------

(define standard-cursors (make-hasheq))
(define cursor-kinds
  (hasheq 'arrow GLFW_ARROW_CURSOR
          'ibeam GLFW_IBEAM_CURSOR
          'crosshair GLFW_CROSSHAIR_CURSOR
          'hand GLFW_HAND_CURSOR
          'hresize GLFW_HRESIZE_CURSOR
          'vresize GLFW_VRESIZE_CURSOR))

;; kind: (or/c 'arrow 'ibeam 'crosshair 'hand 'hresize 'vresize #f)
(define (pw-cursor! pw kind)
  (define c
    (and kind
         (hash-ref! standard-cursors kind
                    (λ () (glfwCreateStandardCursor (hash-ref cursor-kinds kind))))))
  (glfwSetCursor (platform-window-glfw pw) c))
