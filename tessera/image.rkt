#lang racket/base

;; Image codecs for tessera, in pure Racket.
;;
;; Writing: PNG (uncompressed deflate — fine for screenshots and snapshot
;; tests, where size is irrelevant and zero dependencies matter).
;; Reading: QOI (primary), BMP and TGA (simple uncompressed variants).
;; PNG *reading* is deliberately out of scope; convert once at build time if
;; needed (any tool can emit QOI, and it is trivially scriptable).

(require racket/bytes
         racket/file)

(provide png-write
         crc32
         adler32
         qoi-encode
         qoi-decode
         bmp-decode
         tga-decode
         image-load
         argb->rgba!)

;; ---- crc32 / adler32 -----------------------------------------------------------

(define crc-table
  (let ([t (make-vector 256 0)])
    (for ([n (in-range 256)])
      (let loop ([c n] [k 0])
        (cond
          [(= k 8) (vector-set! t n c)]
          [else (loop (bitwise-and #xFFFFFFFF
                                   (if (odd? c) (bitwise-xor #xEDB88320 (arithmetic-shift c -1))
                                       (arithmetic-shift c -1)))
                      (add1 k))])))
    t))

(define (crc32 bs [start 0] [end (bytes-length bs)])
  (let loop ([i start] [c #xFFFFFFFF])
    (if (= i end)
        (bitwise-xor c #xFFFFFFFF)
        (loop (add1 i)
              (bitwise-and #xFFFFFFFF
                           (bitwise-xor (vector-ref crc-table
                                                    (bitwise-and #xFF
                                                                 (bitwise-xor c (bytes-ref bs i))))
                                        (arithmetic-shift c -8)))))))

(define (adler32 bs [start 0] [end (bytes-length bs)])
  (let loop ([i start] [a 1] [b 0])
    (if (= i end)
        (bitwise-ior (arithmetic-shift b 16) a)
        (loop (add1 i)
              (remainder (+ a (bytes-ref bs i)) 65521)
              (remainder (+ b (remainder (+ a (bytes-ref bs i)) 65521)) 65521)))))

;; ---- little-endian / big-endian helpers ----------------------------------------

(define (u32be bs i)
  (+ (arithmetic-shift (bytes-ref bs i) 24)
     (arithmetic-shift (bytes-ref bs (+ i 1)) 16)
     (arithmetic-shift (bytes-ref bs (+ i 2)) 8)
     (bytes-ref bs (+ i 3))))

(define (u16le bs i)
  (+ (bytes-ref bs i) (arithmetic-shift (bytes-ref bs (+ i 1)) 8)))

(define (u32le bs i)
  (+ (bytes-ref bs i)
     (arithmetic-shift (bytes-ref bs (+ i 1)) 8)
     (arithmetic-shift (bytes-ref bs (+ i 2)) 16)
     (arithmetic-shift (bytes-ref bs (+ i 3)) 24)))

(define (u32be! bs i v)
  (bytes-set! bs i (bitwise-and #xFF (arithmetic-shift v -24)))
  (bytes-set! bs (+ i 1) (bitwise-and #xFF (arithmetic-shift v -16)))
  (bytes-set! bs (+ i 2) (bitwise-and #xFF (arithmetic-shift v -8)))
  (bytes-set! bs (+ i 3) (bitwise-and #xFF v)))

(define (u16le! bs i v)
  (bytes-set! bs i (bitwise-and #xFF v))
  (bytes-set! bs (+ i 1) (bitwise-and #xFF (arithmetic-shift v -8))))

(define (u32le! bs i v)
  (bytes-set! bs i (bitwise-and #xFF v))
  (bytes-set! bs (+ i 1) (bitwise-and #xFF (arithmetic-shift v -8)))
  (bytes-set! bs (+ i 2) (bitwise-and #xFF (arithmetic-shift v -16)))
  (bytes-set! bs (+ i 3) (bitwise-and #xFF (arithmetic-shift v -24))))

;; ---- PNG writing -----------------------------------------------------------------

(define png-signature (bytes 137 80 78 71 13 10 26 10))

(define (png-chunk type payload)
  (define len (bytes-length payload))
  (define out (make-bytes (+ 4 len 4 4)))
  (u32be! out 0 len)
  (bytes-copy! out 4 type)
  (bytes-copy! out 8 payload 0 len)
  (u32be! out (+ 8 len) (crc32 out 4 (+ 8 len)))
  out)

;; Write an 8-bit RGBA PNG. `pixels` is width*height*4 bytes, row-major,
;; top-left origin, no row padding.
(define (png-write path width height pixels)
  (define ihdr (make-bytes 13))
  (u32be! ihdr 0 width)
  (u32be! ihdr 4 height)
  (bytes-set! ihdr 8 8)   ; bit depth
  (bytes-set! ihdr 9 6)   ; color type RGBA
  (bytes-set! ihdr 10 0)  ; compression
  (bytes-set! ihdr 11 0)  ; filter
  (bytes-set! ihdr 12 0)  ; interlace
  ;; raw scanlines with filter byte 0
  (define stride (* width 4))
  (define raw (make-bytes (* height (add1 stride))))
  (for ([y (in-range height)])
    (bytes-set! raw (* y (add1 stride)) 0)
    (bytes-copy! raw (add1 (* y (add1 stride))) pixels (* y stride) (* (add1 y) stride)))
  ;; zlib stream: header + stored deflate blocks + adler32
  (define n (bytes-length raw))
  (define n-blocks (ceiling (/ n 65535)))
  (define z (make-bytes (+ 2 n (* 5 (max 1 n-blocks)) 4)))
  (bytes-set! z 0 #x78)
  (bytes-set! z 1 #x01)
  (let loop ([pos 2] [off 0])
    (cond
      [(>= off n)
       (when (= off 0)   ; empty input still needs one final empty block
         (bytes-set! z 2 1)
         (bytes-set! z 3 0) (bytes-set! z 4 #xFF)
         (bytes-set! z 5 0) (bytes-set! z 6 #xFF)
         (set! pos 7))
       (u32be! z pos (adler32 raw))]
      [else
       (define len (min 65535 (- n off)))
       (define final? (<= (+ off len) n))
       (bytes-set! z pos (if final? 1 0))
       (bytes-set! z (+ pos 1) (bitwise-and #xFF len))
       (bytes-set! z (+ pos 2) (bitwise-and #xFF (arithmetic-shift len -8)))
       (bytes-set! z (+ pos 3) (bitwise-and #xFF (bitwise-xor len #xFFFF)))
       (bytes-set! z (+ pos 4) (bitwise-and #xFF (arithmetic-shift (bitwise-xor len #xFFFF) -8)))
       (bytes-copy! z (+ pos 5) raw off (+ off len))
       (loop (+ pos 5 len) (+ off len))]))
  (define out
    (bytes-append png-signature
                  (png-chunk #"IHDR" ihdr)
                  (png-chunk #"IDAT" z)
                  (png-chunk #"IEND" #"")))
  (with-output-to-file path
    (λ () (write-bytes out))
    #:mode 'binary #:exists 'replace))

;; ---- QOI --------------------------------------------------------------------------

(define qoi-magic #"qoif")

(define (qoi-encode width height pixels [channels 3])
  (define n (* width height channels))
  (define max-out (+ 14 n n (quotient n 8) 8)) ; generous bound
  (define out (make-bytes max-out))
  (define pos
    (begin
      (bytes-copy! out 0 qoi-magic)
      (u32be! out 4 width) (u32be! out 8 height)
      (bytes-set! out 12 channels) (bytes-set! out 13 0) ; sRGB
      14))
  (define index (make-bytes (* 64 4)))
  (define (hash-px r g b a)
    (bitwise-and #x3F
                 (+ r (* g 2) (* b 4) (* a 8) 39)))
  (define px (make-bytes 4))
  (bytes-set! px 0 0) (bytes-set! px 1 0) (bytes-set! px 2 0) (bytes-set! px 3 255)
  (define run 0)
  (define (flush-run!)
    (when (> run 0)
      (bytes-set! out pos (bitwise-ior #xC0 (- run 1)))
      (set! pos (add1 pos))
      (set! run 0)))
  (let row-loop ([i 0])
    (unless (= i n)
      (define r (bytes-ref pixels i))
      (define g (bytes-ref pixels (+ i 1)))
      (define b (bytes-ref pixels (+ i 2)))
      (define a (if (= channels 4) (bytes-ref pixels (+ i 3)) 255))
      (cond
        [(and (= r (bytes-ref px 0)) (= g (bytes-ref px 1))
              (= b (bytes-ref px 2)) (= a (bytes-ref px 3)))
         (set! run (add1 run))
         (when (= run 62) (flush-run!))]
        [else
         (flush-run!)
         (define hidx (* 4 (hash-px r g b a)))
         (cond
           [(and (= r (bytes-ref index hidx)) (= g (bytes-ref index (+ hidx 1)))
                 (= b (bytes-ref index (+ hidx 2))) (= a (bytes-ref index (+ hidx 3))))
            (bytes-set! out pos (quotient hidx 4))
            (set! pos (add1 pos))]
           [else
            (bytes-set! index hidx r)
            (bytes-set! index (+ hidx 1) g)
            (bytes-set! index (+ hidx 2) b)
            (bytes-set! index (+ hidx 3) a)
            (cond
              [(= a (bytes-ref px 3))
               (define vr (- r (bytes-ref px 0)))
               (define vg (- g (bytes-ref px 1)))
               (define vb (- b (bytes-ref px 2)))
               (define dg (- vg vr))
               (define db (- vb vg))
               (cond
                 [(and (<= -2 vr 1) (<= -2 vg 1) (<= -2 vb 1))
                  (bytes-set! out pos
                              (bitwise-ior #x40
                                           (arithmetic-shift (+ vr 2) 4)
                                           (arithmetic-shift (+ vg 2) 2)
                                           (+ vb 2)))
                  (set! pos (add1 pos))]
                 [(and (<= -32 dg 31) (<= -8 (- vr dg) 7) (<= -8 (- db dg) 7))
                  (bytes-set! out pos (bitwise-ior #x80 (arithmetic-shift (+ dg 32) 2)))
                  (bytes-set! out (+ pos 1)
                              (bitwise-ior (arithmetic-shift (+ (- vr dg) 8) 4)
                                           (+ (- db dg) 8)))
                  (set! pos (+ pos 2))]
                 [else
                  (bytes-set! out pos #xFE)
                  (bytes-set! out (+ pos 1) r)
                  (bytes-set! out (+ pos 2) g)
                  (bytes-set! out (+ pos 3) b)
                  (set! pos (+ pos 4))])]
              [else
               (bytes-set! out pos #xFF)
               (bytes-set! out (+ pos 1) r)
               (bytes-set! out (+ pos 2) g)
               (bytes-set! out (+ pos 3) b)
               (bytes-set! out (+ pos 4) a)
               (set! pos (+ pos 5))])])
         (bytes-set! px 0 r) (bytes-set! px 1 g)
         (bytes-set! px 2 b) (bytes-set! px 3 a)])
      (row-loop (+ i channels))))
  (flush-run!)
  ;; 8-byte end marker
  (bytes-set! out pos 0) (bytes-set! out (+ pos 1) 0)
  (bytes-set! out (+ pos 2) 0) (bytes-set! out (+ pos 3) 0)
  (bytes-set! out (+ pos 4) 0) (bytes-set! out (+ pos 5) 0)
  (bytes-set! out (+ pos 6) 0) (bytes-set! out (+ pos 7) 1)
  (subbytes out 0 (+ pos 8)))

(define (qoi-decode bs)
  (unless (and (> (bytes-length bs) 14) (equal? (subbytes bs 0 4) qoi-magic))
    (error 'qoi-decode "not a QOI image"))
  (define width (u32be bs 4))
  (define height (u32be bs 8))
  (define channels (bytes-ref bs 12))
  (define pixels (make-bytes (* width height channels)))
  (define index (make-bytes (* 64 4)))
  (define px (bytes 0 0 0 255))
  (define (index-slot)
    (* 4 (bitwise-and #x3F
                      (+ (bytes-ref px 0) (* (bytes-ref px 1) 2)
                         (* (bytes-ref px 2) 4) (* (bytes-ref px 3) 8) 39))))
  (define (remember!)
    (bytes-copy! index (index-slot) px))
  (define (store! i)
    (bytes-copy! pixels i px 0 channels))
  (let loop ([pos 14] [i 0])
    (cond
      [(or (>= i (bytes-length pixels)) (>= pos (bytes-length bs)))
       (values width height pixels)]
      [else
       (define op (bytes-ref bs pos))
       (cond
         [(= op #xFE)                      ; QOI_OP_RGB
          (bytes-set! px 0 (bytes-ref bs (+ pos 1)))
          (bytes-set! px 1 (bytes-ref bs (+ pos 2)))
          (bytes-set! px 2 (bytes-ref bs (+ pos 3)))
          (remember!) (store! i)
          (loop (+ pos 4) (+ i channels))]
         [(= op #xFF)                      ; QOI_OP_RGBA
          (bytes-set! px 0 (bytes-ref bs (+ pos 1)))
          (bytes-set! px 1 (bytes-ref bs (+ pos 2)))
          (bytes-set! px 2 (bytes-ref bs (+ pos 3)))
          (bytes-set! px 3 (bytes-ref bs (+ pos 4)))
          (remember!) (store! i)
          (loop (+ pos 5) (+ i channels))]
         [(< op #x40)                      ; QOI_OP_INDEX
          (bytes-copy! px 0 index (* 4 op) (+ (* 4 op) 4))
          (store! i)
          (loop (+ pos 1) (+ i channels))]
         [(< op #x80)                      ; QOI_OP_DIFF
          (bytes-set! px 0 (bitwise-and #xFF (+ (bytes-ref px 0) (- (bitwise-and (arithmetic-shift op -4) #x03) 2))))
          (bytes-set! px 1 (bitwise-and #xFF (+ (bytes-ref px 1) (- (bitwise-and (arithmetic-shift op -2) #x03) 2))))
          (bytes-set! px 2 (bitwise-and #xFF (+ (bytes-ref px 2) (- (bitwise-and op #x03) 2))))
          (remember!) (store! i)
          (loop (+ pos 1) (+ i channels))]
         [(< op #xC0)                      ; QOI_OP_LUMA
          (define b2 (bytes-ref bs (+ pos 1)))
          (define dg (- (bitwise-and op #x3F) 32))
          (define dr (+ dg (- (bitwise-and (arithmetic-shift b2 -4) #x0F) 8)))
          (define db (+ dg (- (bitwise-and b2 #x0F) 8)))
          (bytes-set! px 0 (bitwise-and #xFF (+ (bytes-ref px 0) dr)))
          (bytes-set! px 1 (bitwise-and #xFF (+ (bytes-ref px 1) dg)))
          (bytes-set! px 2 (bitwise-and #xFF (+ (bytes-ref px 2) db)))
          (remember!) (store! i)
          (loop (+ pos 2) (+ i channels))]
         [else                             ; QOI_OP_RUN
          (define run (+ 1 (bitwise-and op #x3F)))
          (define n (min run (quotient (- (bytes-length pixels) i) channels)))
          (for ([k (in-range n)])
            (store! (+ i (* k channels))))
          (loop (+ pos 1) (+ i (* n channels)))])])))

;; ---- BMP --------------------------------------------------------------------------

;; Minimal uncompressed 24/32-bit BITMAPINFOHEADER support.
(define (bmp-decode bs)
  (unless (and (> (bytes-length bs) 54)
               (= (bytes-ref bs 0) 66) (= (bytes-ref bs 1) 77))
    (error 'bmp-decode "not a BMP image"))
  (define offset (u32le bs 10))
  (define header-size (u32le bs 14))
  (unless (>= header-size 40)
    (error 'bmp-decode "unsupported BMP header"))
  (define width (integer-bytes->integer (subbytes bs 18 22) #f #t))
  (define raw-height (integer-bytes->integer (subbytes bs 22 26) #t #t))  ; signed
  (define bpp (u16le bs 28))
  (define compression (u32le bs 30))
  (unless (= compression 0)
    (error 'bmp-decode "compressed BMP not supported"))
  (define height (abs raw-height))
  (define bottom-up (> raw-height 0))
  (define channels (case bpp [(32) 4] [(24) 3] [else (error 'bmp-decode "BPP ~a not supported" bpp)]))
  (define pixels (make-bytes (* width height 4)))
  (define row-size (* (ceiling (/ (* width bpp) 32)) 4))
  (for ([y (in-range height)])
    (define src-y (if bottom-up (- height 1 y) y))
    (define row-base (+ offset (* src-y row-size)))
    (for ([x (in-range width)])
      (define si (+ row-base (* x channels)))
      (define di (+ (* y width 4) (* x 4)))
      (bytes-set! pixels di (bytes-ref bs (+ si 2)))
      (bytes-set! pixels (+ di 1) (bytes-ref bs (+ si 1)))
      (bytes-set! pixels (+ di 2) (bytes-ref bs si))
      (bytes-set! pixels (+ di 3) (if (= channels 4) (bytes-ref bs (+ si 3)) 255))))
  (values width height pixels))

;; ---- TGA --------------------------------------------------------------------------

;; Uncompressed true-color (type 2) with 24 or 32 bpp.
(define (tga-decode bs)
  (unless (>= (bytes-length bs) 18)
    (error 'tga-decode "not a TGA image"))
  (define id-length (bytes-ref bs 0))
  (define type (bytes-ref bs 2))
  (unless (= type 2)
    (error 'tga-decode "only uncompressed TGA supported"))
  (define width (u16le bs 12))
  (define height (u16le bs 14))
  (define bpp (bytes-ref bs 16))
  (define channels (case bpp [(32) 4] [(24) 3] [else (error 'tga-decode "BPP ~a not supported" bpp)]))
  (define descriptor (bytes-ref bs 17))
  (define top-origin (bitwise-bit-set? descriptor 5))
  (define base (+ 18 id-length))
  (define pixels (make-bytes (* width height 4)))
  (for ([y (in-range height)])
    (define src-y (if top-origin y (- height 1 y)))
    (for ([x (in-range width)])
      (define si (+ base (* src-y width channels) (* x channels)))
      (define di (+ (* y width 4) (* x 4)))
      (bytes-set! pixels di (bytes-ref bs (+ si 2)))
      (bytes-set! pixels (+ di 1) (bytes-ref bs (+ si 1)))
      (bytes-set! pixels (+ di 2) (bytes-ref bs si))
      (bytes-set! pixels (+ di 3) (if (= channels 4) (bytes-ref bs (+ si 3)) 255))))
  (values width height pixels))

;; ---- dispatch ---------------------------------------------------------------------

(define (image-load path)
  (define bs (file->bytes path))
  (cond
    [(and (> (bytes-length bs) 14) (equal? (subbytes bs 0 4) qoi-magic))
     (qoi-decode bs)]
    [(and (> (bytes-length bs) 2) (= (bytes-ref bs 0) 66) (= (bytes-ref bs 1) 77))
     (bmp-decode bs)]
    ;; TGA has no magic; the image-type byte of an uncompressed true-color
    ;; file is 2, which is distinctive enough after the magic checks above.
    [(and (> (bytes-length bs) 18) (= (bytes-ref bs 2) 2))
     (tga-decode bs)]
    [else (error 'image-load "unsupported image format: ~a" path)]))

;; Helper: an ARGB (0xAARRGGBB) integer list into an RGBA byte string, used
;; by the theme tests and tooling.
(define (argb->rgba! rgba argb-list)
  (for ([argb (in-list argb-list)] [i (in-naturals)])
    (define di (* i 4))
    (bytes-set! rgba di (bitwise-and #xFF (arithmetic-shift argb -16)))
    (bytes-set! rgba (+ di 1) (bitwise-and #xFF (arithmetic-shift argb -8)))
    (bytes-set! rgba (+ di 2) (bitwise-and #xFF argb))
    (bytes-set! rgba (+ di 3) (bitwise-and #xFF (arithmetic-shift argb -24)))))
