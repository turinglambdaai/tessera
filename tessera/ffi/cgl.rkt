#lang racket/base

;; CGL bindings for tessera's macOS context backend.
;;
;; GLFW's NSGL layer cannot reliably create core-profile contexts inside a
;; Racket process (its NSOpenGLPixelFormat Objective-C path fails there),
;; but the C API underneath — CGL — works fine. So on macOS tessera opens
;; the window with GLFW (GLFW_CLIENT_API = NO_API) and builds the OpenGL
;; context directly with CGL, then attaches it to the window's content view
;; through NSOpenGLContext's initWithCGLContextObj:.
;;
;; Library resolution is lazy: requiring this module is harmless on every
;; platform; OpenGL.framework is only touched on first use (which
;; tessera/platform.rkt does only on macOS).

(require ffi/unsafe)

(provide cgl-create-core-context!
         cgl-current!
         cgl-flush!
         cgl-set-vsync!
         cgl-release!)

;; ---- constants (values from CGLTypes.h) --------------------------------------

(define kCGLPFAOpenGLProfile 99)
(define kCGLPFADoubleBuffer 5)
(define kCGLOGLPVersion_3_2_Core #x3200)   ; negotiates 3.2..4.1 core
(define kCGLCPSwapInterval 222)

;; ---- lazy library --------------------------------------------------------------

(define libgl #f)
(define cache (make-hasheq))

(define-cpointer-type _CGLPixelFormatObj)
(define-cpointer-type _CGLContextObj)

(define (ensure-libgl!)
  (unless libgl
    (set! libgl (ffi-lib "/System/Library/Frameworks/OpenGL.framework/OpenGL")))
  libgl)

(define (cfn sym ty)
  (hash-ref! cache sym (λ () (get-ffi-obj sym (ensure-libgl!) ty))))

(define CGLChoosePixelFormat-ty
  (_fun [attrs : _pointer]
        [pix : (_ptr o _CGLPixelFormatObj)]
        [npix : (_ptr o _int)]
        -> [err : _int]
        -> (values pix npix err)))
(define CGLCreateContext-ty
  (_fun _CGLPixelFormatObj _CGLContextObj/null
        [ctx : (_ptr o _CGLContextObj)]
        -> [err : _int]
        -> (values err ctx)))
(define CGLReleasePixelFormat-ty (_fun _CGLPixelFormatObj -> _int))
(define CGLReleaseContext-ty     (_fun _CGLContextObj -> _int))
(define CGLSetCurrentContext-ty  (_fun _CGLContextObj -> _int))
(define CGLFlushDrawable-ty      (_fun _CGLContextObj -> _int))
(define CGLSetParameter-ty       (_fun _CGLContextObj _int _pointer -> _int))

(define (check err who)
  (unless (zero? err)
    (error 'tessera/cgl "~a failed with CGL error ~a" who err)))

;; ---- API -----------------------------------------------------------------------

;; Create a double-buffered OpenGL core-profile context and return the raw
;; CGLContextObj. The caller attaches it to a view; see tessera/platform.rkt.
(define (cgl-create-core-context!)
  (define attrs (malloc _int 4 'raw))
  (ptr-set! attrs _int 0 kCGLPFAOpenGLProfile)
  (ptr-set! attrs _int 1 kCGLOGLPVersion_3_2_Core)
  (ptr-set! attrs _int 2 kCGLPFADoubleBuffer)
  (ptr-set! attrs _int 3 0)                       ; terminator
  (define-values (pix npix err)
    ((cfn 'CGLChoosePixelFormat CGLChoosePixelFormat-ty) attrs))
  (check err "CGLChoosePixelFormat")
  (when (zero? npix)
    (error 'tessera/cgl "no core-profile pixel format available on this system"))
  (define-values (err2 ctx)
    ((cfn 'CGLCreateContext CGLCreateContext-ty) pix #f))
  ((cfn 'CGLReleasePixelFormat CGLReleasePixelFormat-ty) pix) ; ctx holds its own ref
  (check err2 "CGLCreateContext")
  ctx)

(define (cgl-current! ctx)
  (check ((cfn 'CGLSetCurrentContext CGLSetCurrentContext-ty) ctx) "CGLSetCurrentContext"))

(define (cgl-flush! ctx)
  (check ((cfn 'CGLFlushDrawable CGLFlushDrawable-ty) ctx) "CGLFlushDrawable"))

(define (cgl-set-vsync! ctx on?)
  (define v (malloc _int 1 'raw))
  (ptr-set! v _int 0 (if on? 1 0))
  (check ((cfn 'CGLSetParameter CGLSetParameter-ty) ctx kCGLCPSwapInterval v)
         "CGLSetParameter(swap interval)"))

(define (cgl-release! ctx)
  (check ((cfn 'CGLReleaseContext CGLReleaseContext-ty) ctx) "CGLReleaseContext"))
