#lang racket/base

;; tessera's platform layer: window creation, GL context lifecycle, event
;; pump, clipboard, and cursors — one API for every supported OS.
;;
;; Two context backends, chosen by OS:
;;
;;   macOS  GLFW opens a NO_API window (windowing only); the OpenGL core
;;          context is created with CGL and attached to the window's content
;;          view through the Objective-C runtime. GLFW's own NSGL path is
;;          unreliable inside a Racket process; CGL gives us the same core
;;          profile everywhere.
;;
;;   other  GLFW creates the OpenGL 3.3-core context itself (the default
;;          path; exercised on Linux and Windows).
;;
;; Everything above this layer (renderer, text, UI) is pure Racket. The
;; mac-only ffi modules resolve their libraries lazily, so requiring them
;; here is safe on every platform.

(require ffi/unsafe
         racket/bool
         racket/promise
         "ffi/glfw.rkt"
         "ffi/gl.rkt"
         "ffi/objc.rkt"
         "ffi/cgl.rkt")

(provide platform-window?
         platform-window-glfw
         platform-window-kind
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

(struct platform-window (glfw        ; GLFWwindow handle
                         context     ; CGLContextObj on macOS, #f elsewhere
                         nsctx       ; NSOpenGLContext on macOS, #f elsewhere
                         kind)       ; 'cgl | 'glfw-gl
  #:transparent)

(define macos? (eq? (system-type 'os) 'macosx))
(define initialized? #f)

;; GLFW native access handle (macOS only), resolved on first use.
(define glfw-get-cocoa-window
  (delay
    (get-ffi-obj "glfwGetCocoaWindow" glfw-lib (_fun _GLFWwindow -> _pointer))))

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
  (glfwDefaultWindowHints)
  (glfwWindowHint GLFW_VISIBLE (if visible? GLFW_TRUE GLFW_FALSE))
  (glfwWindowHint GLFW_RESIZABLE (if resizable? GLFW_TRUE GLFW_FALSE))
  (cond
    [macos?
     ;; Windowing only; the context is created via CGL below.
     (glfwWindowHint GLFW_CLIENT_API GLFW_NO_API)]
    [else
     (glfwWindowHint GLFW_CLIENT_API GLFW_OPENGL_API)
     (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 3)
     (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 3)
     (glfwWindowHint GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)
     (glfwWindowHint GLFW_OPENGL_FORWARD_COMPAT GLFW_TRUE)])
  (define win (glfwCreateWindow width height title #f #f))
  (unless win
    (error 'tessera "failed to create a window (is a display available?)"))
  (when (and min-width min-height)
    (glfwSetWindowSizeLimits win min-width min-height GLFW_DONT_CARE GLFW_DONT_CARE))
  (define pw
    (cond
      [macos?
       (define ctx (cgl-create-core-context!))
       (define nswin ((force glfw-get-cocoa-window) win))
       (define content-view (msg-send/id nswin (objc-sel "contentView")))
       (define nsctx
         (msg-send/id1 (msg-send/id (objc-class "NSOpenGLContext") (objc-sel "alloc"))
                       (objc-sel "initWithCGLContextObj:")
                       ctx))
       (unless nsctx
         (error 'tessera "NSOpenGLContext initWithCGLContextObj: failed"))
       (msg-send/void1 nsctx (objc-sel "setView:") content-view)
       (platform-window win ctx nsctx 'cgl)]
      [else
       (glfwMakeContextCurrent win)
       (platform-window win #f #f 'glfw-gl)]))
  (install-gl-loader! pw)
  (pw-vsync! pw #t)
  (when maximize (glfwMaximizeWindow win))
  pw)

(define (close-platform-window! pw)
  ;; The NSOpenGLContext adopted the CGL context at creation (it owns the
  ;; final release), so only the Cocoa side is released here.
  (glfwDestroyWindow (platform-window-glfw pw))
  (when (platform-window-nsctx pw)
    (msg-send/id (platform-window-nsctx pw) (objc-sel "release"))))

;; ---- GL plumbing --------------------------------------------------------------

;; Point the GL loader at the right resolution strategy for this context.
(define (install-gl-loader! pw)
  (cond
    [(eq? (platform-window-kind pw) 'cgl)
     ;; dlsym straight from OpenGL.framework: every core entry point tessera
     ;; uses is exported there and dispatches through the current CGL context.
     (define libgl (ffi-lib "/System/Library/Frameworks/OpenGL.framework/OpenGL"))
     (gl-set-loader!
      (λ (name)
        (with-handlers ([exn:fail? (λ (_) #f)])
          (get-ffi-obj name libgl _fpointer))))]
    [else
     (gl-set-loader! (λ (name) (glfwGetProcAddress name)))]))

;; ---- per-frame GL --------------------------------------------------------------

(define (pw-make-current! pw)
  (cond
    [(eq? (platform-window-kind pw) 'cgl)
     (cgl-current! (platform-window-context pw))]
    [else (glfwMakeContextCurrent (platform-window-glfw pw))]))

(define (pw-swap! pw)
  (cond
    [(eq? (platform-window-kind pw) 'cgl)
     (cgl-flush! (platform-window-context pw))]
    [else (glfwSwapBuffers (platform-window-glfw pw))]))

(define (pw-vsync! pw on?)
  (cond
    [(eq? (platform-window-kind pw) 'cgl)
     (cgl-set-vsync! (platform-window-context pw) on?)]
    [else (glfwSwapInterval (if on? 1 0))]))

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
