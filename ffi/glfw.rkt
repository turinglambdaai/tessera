#lang racket/base

;; GLFW3 bindings for tessera.
;;
;; Loads the system GLFW shared library at runtime — no linking step and no
;; C toolchain on the user side. All higher-level window handling lives in
;; tessera/app.rkt; this module stays close to the C API on purpose so new
;; entry points are one line each.

(require ffi/unsafe
         ffi/unsafe/define)

(provide (all-defined-out))

;; ---- library discovery ----------------------------------------------------

(define glfw-candidates
  (list "/opt/homebrew/lib/libglfw.3.dylib"        ; Homebrew, Apple silicon
        "/usr/local/lib/libglfw.3.dylib"           ; Homebrew, Intel
        "libglfw.3.dylib"                          ; DYLD/LD paths, macOS
        "libglfw.so.3"                             ; Debian/Ubuntu, Fedora
        "libglfw.so"))

(define glfw-lib
  (let loop ([cs glfw-candidates])
    (cond
      [(null? cs)
       (error 'tessera
               (string-append
                "GLFW 3 was not found. Install it first:\n"
                "  macOS:          brew install glfw\n"
                "  Debian/Ubuntu:  apt install libglfw3\n"
                "  Fedora:         dnf install glfw-devel"))]
      [else
       (with-handlers ([exn:fail:filesystem? (λ (_) (loop (cdr cs)))])
         (ffi-lib (car cs)))])))

(define-ffi-definer define-glfw glfw-lib #:provide provide)

;; ---- opaque handles --------------------------------------------------------

(define-cpointer-type _GLFWwindow)
(define-cpointer-type _GLFWmonitor)
(define-cpointer-type _GLFWcursor)

;; ---- constants (values from glfw3.h) ---------------------------------------

(define GLFW_FALSE 0)
(define GLFW_TRUE 1)
(define GLFW_DONT_CARE -1)

(define GLFW_FOCUSED            #x00020001)
(define GLFW_ICONIFIED          #x00020002)
(define GLFW_RESIZABLE          #x00020003)
(define GLFW_VISIBLE            #x00020004)
(define GLFW_TRANSPARENT_FRAMEBUFFER #x0002000A)
(define GLFW_SAMPLES #x0002100D)

(define GLFW_CLIENT_API         #x00022001)
(define GLFW_NO_API             0)
(define GLFW_OPENGL_API         #x00030001)
(define GLFW_CONTEXT_VERSION_MAJOR #x00022004)
(define GLFW_CONTEXT_VERSION_MINOR #x00022005)
(define GLFW_OPENGL_FORWARD_COMPAT #x00022007)
(define GLFW_OPENGL_PROFILE        #x00022006)
(define GLFW_OPENGL_CORE_PROFILE   #x00032001)

(define GLFW_PRESS   1)
(define GLFW_RELEASE 0)
(define GLFW_REPEAT  2)

(define GLFW_MOD_SHIFT   #x0001)
(define GLFW_MOD_CONTROL #x0002)
(define GLFW_MOD_ALT     #x0004)
(define GLFW_MOD_SUPER   #x0008)

(define GLFW_MOUSE_BUTTON_1 0)   ; left
(define GLFW_MOUSE_BUTTON_2 1)   ; right

(define GLFW_KEY_SPACE      32)
(define GLFW_KEY_ESCAPE     256)
(define GLFW_KEY_ENTER      257)
(define GLFW_KEY_TAB        258)
(define GLFW_KEY_BACKSPACE  259)
(define GLFW_KEY_INSERT     260)
(define GLFW_KEY_DELETE     261)
(define GLFW_KEY_RIGHT      262)
(define GLFW_KEY_LEFT       263)
(define GLFW_KEY_DOWN       264)
(define GLFW_KEY_UP         265)
(define GLFW_KEY_PAGE_UP    266)
(define GLFW_KEY_PAGE_DOWN  267)
(define GLFW_KEY_HOME       268)
(define GLFW_KEY_END        269)
(define GLFW_KEY_LEFT_SHIFT   340)
(define GLFW_KEY_LEFT_CONTROL 341)
(define GLFW_KEY_LEFT_ALT     342)
(define GLFW_KEY_LEFT_SUPER   343)

(define GLFW_ARROW_CURSOR   #x00036001)
(define GLFW_IBEAM_CURSOR   #x00036002)
(define GLFW_CROSSHAIR_CURSOR #x00036003)
(define GLFW_HAND_CURSOR    #x00036004)
(define GLFW_HRESIZE_CURSOR #x00036005)
(define GLFW_VRESIZE_CURSOR #x00036006)

;; ---- core -------------------------------------------------------------------

(define-glfw glfwInit           (_fun -> _int))
(define-glfw glfwTerminate      (_fun -> _void))
(define-glfw glfwGetVersionString (_fun -> _string/utf-8))
(define-glfw glfwGetError (_fun [desc : (_ptr o _pointer)] -> _int -> (values desc)))
(define-glfw glfwGetTime        (_fun -> _double))
(define-glfw glfwSetTime        (_fun _double -> _void))
(define-glfw glfwPollEvents     (_fun -> _void))
(define-glfw glfwWaitEvents     (_fun -> _void))
(define-glfw glfwWaitEventsTimeout (_fun _double -> _void))
(define-glfw glfwPostEmptyEvent (_fun -> _void))

;; ---- window -----------------------------------------------------------------

(define-glfw glfwDefaultWindowHints (_fun -> _void))
(define-glfw glfwWindowHint         (_fun _int _int -> _void))
(define-glfw glfwCreateWindow       (_fun _int _int _string/utf-8 _GLFWmonitor/null _GLFWwindow/null -> _GLFWwindow/null))
(define-glfw glfwDestroyWindow      (_fun _GLFWwindow -> _void))
(define-glfw glfwWindowShouldClose  (_fun _GLFWwindow -> _int))
(define-glfw glfwSetWindowShouldClose (_fun _GLFWwindow _int -> _void))
(define-glfw glfwShowWindow         (_fun _GLFWwindow -> _void))
(define-glfw glfwSetWindowTitle     (_fun _GLFWwindow _string/utf-8 -> _void))
(define-glfw glfwGetWindowSize      (_fun _GLFWwindow [w : (_ptr o _int)] [h : (_ptr o _int)] -> _void -> (values w h)))
(define-glfw glfwSetWindowSize      (_fun _GLFWwindow _int _int -> _void))
(define-glfw glfwGetWindowPos       (_fun _GLFWwindow [x : (_ptr o _int)] [y : (_ptr o _int)] -> _void -> (values x y)))
(define-glfw glfwSetWindowPos       (_fun _GLFWwindow _int _int -> _void))
(define-glfw glfwGetFramebufferSize (_fun _GLFWwindow [w : (_ptr o _int)] [h : (_ptr o _int)] -> _void -> (values w h)))
(define-glfw glfwGetWindowContentScale (_fun _GLFWwindow [x : (_ptr o _float)] [y : (_ptr o _float)] -> _void -> (values x y)))
(define-glfw glfwSetWindowSizeLimits (_fun _GLFWwindow _int _int _int _int -> _void))
(define-glfw glfwMaximizeWindow     (_fun _GLFWwindow -> _void))
(define-glfw glfwFocusWindow        (_fun _GLFWwindow -> _void))
(define-glfw glfwRequestWindowAttention (_fun _GLFWwindow -> _void))
(define-glfw glfwGetWindowAttrib    (_fun _GLFWwindow _int -> _int))
(define-glfw glfwMakeContextCurrent (_fun _GLFWwindow -> _void))
(define-glfw glfwSwapBuffers        (_fun _GLFWwindow -> _void))
(define-glfw glfwSwapInterval       (_fun _int -> _void))
(define-glfw glfwExtensionSupported (_fun _string/utf-8 -> _int))
(define-glfw glfwGetProcAddress     (_fun _string/utf-8 -> _fpointer))

;; ---- input ------------------------------------------------------------------

(define-glfw glfwGetCursorPos (_fun _GLFWwindow [x : (_ptr o _double)] [y : (_ptr o _double)] -> _void -> (values x y)))
(define-glfw glfwGetMouseButton (_fun _GLFWwindow _int -> _int))
(define-glfw glfwSetClipboardString (_fun _GLFWwindow _string/utf-8 -> _void))
(define-glfw glfwGetClipboardString (_fun _GLFWwindow -> _pointer))
(define-glfw glfwCreateStandardCursor (_fun _int -> _GLFWcursor/null))
(define-glfw glfwDestroyCursor (_fun _GLFWcursor -> _void))
(define-glfw glfwSetCursor (_fun _GLFWwindow _GLFWcursor/null -> _void))
(define-glfw glfwGetKey (_fun _GLFWwindow _int -> _int))

;; ---- callbacks --------------------------------------------------------------
;; Each setter returns the previously-installed callback as a raw pointer.
;; Callers MUST keep the returned callback closures alive (see keep-alive
;; registry in tessera/app.rkt); a collected callback while C still holds it
;; is a crash.

(define-glfw glfwSetErrorCallback
  (_fun (_fun _int _string/utf-8 -> _void) -> _fpointer))
(define-glfw glfwSetWindowCloseCallback
  (_fun _GLFWwindow (_fun _GLFWwindow -> _void) -> _fpointer))
(define-glfw glfwSetFramebufferSizeCallback
  (_fun _GLFWwindow (_fun _GLFWwindow _int _int -> _void) -> _fpointer))
(define-glfw glfwSetKeyCallback
  (_fun _GLFWwindow (_fun _GLFWwindow _int _int _int _int -> _void) -> _fpointer))
(define-glfw glfwSetCharCallback
  (_fun _GLFWwindow (_fun _GLFWwindow _uint32 -> _void) -> _fpointer))
(define-glfw glfwSetMouseButtonCallback
  (_fun _GLFWwindow (_fun _GLFWwindow _int _int _int -> _void) -> _fpointer))
(define-glfw glfwSetCursorPosCallback
  (_fun _GLFWwindow (_fun _GLFWwindow _double _double -> _void) -> _fpointer))
(define-glfw glfwSetScrollCallback
  (_fun _GLFWwindow (_fun _GLFWwindow _double _double -> _void) -> _fpointer))
(define-glfw glfwSetWindowFocusCallback
  (_fun _GLFWwindow (_fun _GLFWwindow _int -> _void) -> _fpointer))
