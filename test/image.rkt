#lang racket/base

;; Image codec tests: QOI round-trips, PNG structure sanity, BMP/TGA decode.

(require rackunit
         racket/file
         tessera/image)

;; ---- QOI round-trip ----------------------------------------------------------

;; gradient + runs + distinct colors: exercises every chunk type
(define (test-pixels w h)
  (define px (make-bytes (* w h 4)))
  (for ([y (in-range h)] [x (in-range w)])
    (define i (* 4 (+ (* y w) x)))
    (bytes-set! px i (modulo (* x 7) 256))
    (bytes-set! px (+ i 1) (modulo (* y 13) 256))
    (bytes-set! px (+ i 2) (modulo (+ x y) 256))
    (bytes-set! px (+ i 3) 255))
  px)

(define px (test-pixels 37 23))
(define encoded (qoi-encode 37 23 px 4))
(define-values (dw dh decoded) (qoi-decode encoded))
(check-equal? dw 37)
(check-equal? dh 23)
(check-equal? decoded px "QOI round-trip must be lossless")

;; alpha-carrying pixels
(define pxa (make-bytes (* 8 8 4)))
(for ([i (in-range 64)])
  (bytes-set! pxa (* i 4) (modulo (* i 31) 256))
  (bytes-set! pxa (+ (* i 4) 3) (if (even? i) 128 255)))
(define-values (da dh2 dpx) (qoi-decode (qoi-encode 8 8 pxa 4)))
(check-equal? dpx pxa "QOI RGBA round-trip must be lossless")

;; long runs (constant image)
(define const-px (make-bytes (* 300 5 4)))
(for ([i (in-range (* 300 5))])
  (bytes-set! const-px (* i 4) 200)
  (bytes-set! const-px (+ (* i 4) 1) 100)
  (bytes-set! const-px (+ (* i 4) 2) 50))
(define-values (cw ch cpx) (qoi-decode (qoi-encode 300 5 const-px 4)))
(check-equal? cpx const-px "long-run round-trip must be lossless")

;; ---- PNG writer ----------------------------------------------------------------

(define tmp (make-temporary-file "tessera-img-~a.png"))
(png-write tmp 4 3 (test-pixels 4 3))
(define written (file->bytes tmp))
(check-equal? (subbytes written 0 8) (bytes 137 80 78 71 13 10 26 10) "PNG signature")
;; chunk walk: length/type/CRC must agree
(define (chunk-at bs pos)
  (define len (integer-bytes->integer bs #f #t pos (+ pos 4)))
  (define type (subbytes bs (+ pos 4) (+ pos 8)))
  (define crc-pos (+ pos 8 len))
  (define crc-in-file (integer-bytes->integer bs #f #t crc-pos (+ crc-pos 4)))
  (check-equal? crc-in-file (crc32 bs (+ pos 4) crc-pos) (format "~a CRC" type))
  (+ crc-pos 4))
(define after-ihdr (chunk-at written 8))
(define after-idat (chunk-at written after-ihdr))
(chunk-at written after-idat)
(delete-file tmp)

;; ---- BMP decode ----------------------------------------------------------------

;; hand-build a 2x2 24bpp bottom-up BMP
(define bmp (make-bytes 70 0))
(bytes-set! bmp 0 66) (bytes-set! bmp 1 77)          ; "BM"
(integer->integer-bytes 70 4 #f #f bmp 2)            ; file size
(integer->integer-bytes 54 4 #f #f bmp 10)           ; pixel offset
(integer->integer-bytes 40 4 #f #f bmp 14)           ; header size
(integer->integer-bytes 2 4 #f #f bmp 18)            ; width
(integer->integer-bytes 2 4 #f #f bmp 22)            ; height (positive = bottom-up)
(integer->integer-bytes 1 2 #f #f bmp 26)            ; planes
(integer->integer-bytes 24 2 #f #f bmp 28)           ; bpp
;; row 1 (bottom): red pixel then green pixel, padded to 8 bytes
(integer->integer-bytes 255 4 #f #f bmp 54)          ; B=255... set channels below
(bytes-set! bmp 54 0) (bytes-set! bmp 55 0) (bytes-set! bmp 56 255)   ; red
(bytes-set! bmp 57 0) (bytes-set! bmp 58 255) (bytes-set! bmp 59 0)   ; green
;; row 0 (top): blue then white
(bytes-set! bmp 62 255) (bytes-set! bmp 63 0) (bytes-set! bmp 64 0)   ; blue
(bytes-set! bmp 65 255) (bytes-set! bmp 66 255) (bytes-set! bmp 67 255) ; white
(define-values (bw bh bp) (bmp-decode bmp))
(check-equal? bw 2)
(check-equal? bh 2)
(check-equal? (subbytes bp 0 16)
              (bytes 0 0 255 255    ; top-left: blue
                     255 255 255 255 ; top-right: white
                     255 0 0 255   ; bottom-left: red
                     0 255 0 255) "bmp pixels") ; bottom-right: green

;; ---- TGA decode ----------------------------------------------------------------

;; 2x1 uncompressed 32bpp, top-origin
(define tga (make-bytes (+ 18 8) 0))
(bytes-set! tga 2 2)
(integer->integer-bytes 2 2 #f #f tga 12)
(integer->integer-bytes 1 2 #f #f tga 14)
(bytes-set! tga 16 32)
(bytes-set! tga 17 32)  ; top-origin bit (0x20) + 8 alpha bits
(bytes-set! tga 18 10) (bytes-set! tga 19 20) (bytes-set! tga 20 30) (bytes-set! tga 21 40)
(bytes-set! tga 22 1) (bytes-set! tga 23 2) (bytes-set! tga 24 3) (bytes-set! tga 25 255)
(define-values (tw th tp) (tga-decode tga))
(check-equal? tw 2)
(check-equal? th 1)
(check-equal? (subbytes tp 0 8) (bytes 30 20 10 40 3 2 1 255))
