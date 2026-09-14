#lang racket/base

;; Minimal Objective-C runtime bindings for tessera's macOS backend.
;;
;; Only what is needed to attach a CGL context to a GLFW window's view:
;; class lookup, selector registration, and objc_msgSend at a few fixed
;; arities. Library resolution is lazy — requiring this module is harmless
;; on every platform; the Objective-C runtime is only touched on first use
;; (which tessera/platform.rkt does only on macOS).

(require ffi/unsafe)

(provide objc-class
         objc-sel
         msg-send/id       ; (obj, sel) -> id
         msg-send/id1      ; (obj, sel, id) -> id
         msg-send/void1    ; (obj, sel, id) -> void
         msg-send/void0)   ; (obj, sel) -> void

(define libobjc #f)
(define cache (make-hash))   ; key: (list symbol type) — same symbol, several arities

(define (ensure-libobjc!)
  (unless libobjc
    (set! libobjc (ffi-lib "/usr/lib/libobjc.dylib")))
  libobjc)

;; Resolve a C symbol once per (symbol, type) pair.
(define (objc-fn sym ty)
  (hash-ref! cache (list sym ty) (λ () (get-ffi-obj sym (ensure-libobjc!) ty))))

(define objc-getClass-ty (_fun _string/utf-8 -> _pointer))
(define sel-registerName-ty (_fun _string/utf-8 -> _pointer))

(define (objc-class name)
  (define p ((objc-fn 'objc_getClass objc-getClass-ty) name))
  (unless p (error 'tessera/objc "Objective-C class not found: ~a" name))
  p)

(define (objc-sel name) ((objc-fn 'sel_registerName sel-registerName-ty) name))

(define msg-send/id
  (λ (obj selector)
    ((objc-fn 'objc_msgSend (_fun _pointer _pointer -> _pointer)) obj selector)))

(define msg-send/id1
  (λ (obj selector arg)
    ((objc-fn 'objc_msgSend (_fun _pointer _pointer _pointer -> _pointer)) obj selector arg)))

(define msg-send/void1
  (λ (obj selector arg)
    ((objc-fn 'objc_msgSend (_fun _pointer _pointer _pointer -> _void)) obj selector arg)))

(define msg-send/void0
  (λ (obj selector)
    ((objc-fn 'objc_msgSend (_fun _pointer _pointer -> _void)) obj selector)))
