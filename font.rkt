#lang racket/base

;; TrueType font parsing for tessera — pure Racket, no C dependencies.
;;
;; Covers what a UI toolkit actually needs:
;;   - TTF files and TTC collections (first font, or by index)
;;   - cmap formats 0 / 4 / 6 / 12 (BMP + astral planes)
;;   - simple and composite glyphs (scale / xy / 2x2 component transforms)
;;   - hhea/hmtx metrics and the legacy kern table (format 0)
;;
;; Glyph outlines are returned in font units as a list of contours, each a
;; list of (x y on-curve?) — TrueType quadratic control points included.
;; Flattening and rasterization live in tessera/text.
;;
;; CFF/PostScript outlines are intentionally out of scope; `find-font-file`
;; (text.rkt) resolves platform fallbacks to TrueType-flavored files.

(require racket/bytes
         racket/format
         racket/file
         racket/format)

(provide (struct-out outline-point)
         (struct-out font)
         load-font
         glyph-outline
         glyph-advance
         glyph-kern
         font-ascent
         font-descent
         font-line-height
         font-units->px)

;; ---- byte readers -----------------------------------------------------------------

(define (u8 bs i) (bytes-ref bs i))
(define (u16 bs i)
  (+ (arithmetic-shift (bytes-ref bs i) 8) (bytes-ref bs (+ i 1))))
(define (s16 bs i)
  (define v (u16 bs i))
  (if (>= v #x8000) (- v #x10000) v))
(define (s8 bs i)
  (define v (u8 bs i))
  (if (>= v #x80) (- v #x100) v))
(define (u32 bs i)
  (+ (arithmetic-shift (bytes-ref bs i) 24)
     (arithmetic-shift (bytes-ref bs (+ i 1)) 16)
     (arithmetic-shift (bytes-ref bs (+ i 2)) 8)
     (bytes-ref bs (+ i 3))))

;; ---- structures ---------------------------------------------------------------------

(struct font (data                ; bytes of this font's table area
              tables              ; tag-string -> (cons offset length)
              units-per-em
              num-glyphs
              index-to-loc-format
              ascender            ; hhea ascender, font units (up positive)
              descender           ; hhea descender (usually negative)
              line-gap
              num-h-metrics
              cmap-lookup         ; char -> glyph index (or #f)
              kerning)            ; hash: (+ (* left 65536) right) -> x advance
  #:transparent)

(define (table-span f tag)
  (hash-ref (font-tables f) tag #f))

;; the raw bytes of a table
(define (table-subbytes f tag)
  (define t (table-span f tag))
  (and t (subbytes (font-data f) (car t) (+ (car t) (cdr t)))))

;; ---- table directory ------------------------------------------------------------------

(define (parse-table-directory data base)
  (define num-tables (u16 data (+ base 4)))
  (let loop ([i 0] [tables (hash)])
    (if (>= i num-tables)
        tables
        (let* ([rec (+ base 12 (* 16 i))]
               [tag (bytes->string/utf-8 (subbytes data rec (+ rec 4)))]
               [off (u32 data (+ rec 8))]
               [len (u32 data (+ rec 12))])
          (loop (add1 i) (hash-set tables tag (cons off len)))))))

;; ---- cmap ------------------------------------------------------------------------------

(define (parse-cmap data tables)
  (define span (hash-ref tables "cmap" #f))
  (unless span
    (error 'load-font "font has no cmap table"))
  (define base (car span))
  (define n (u16 data (+ base 2)))
  ;; prefer (3 . 10) > (3 . 1) > (0 . *) > (3 . 0) — first format we can parse
  (let pick ([i 4] [best #f] [best-rank -1])
    (cond
      [(>= i (+ 4 (* 8 n)))
       (unless best (error 'load-font "no supported cmap subtable"))
       (let ([format (u16 data (+ base best))])
         (case format
           [(4) (make-cmap4 data (+ base best))]
           [(12) (make-cmap12 data (+ base best))]
           [(6) (make-cmap6 data (+ base best))]
           [(0) (make-cmap0 data (+ base best))]
           [else (error 'load-font "unsupported cmap format ~a" format)]))]
      [else
       (define platform (u16 data (+ base i)))
       (define encoding (u16 data (+ base i 2)))
       (define offset (u32 data (+ base i 4)))
       (define rank
         (cond [(and (= platform 3) (= encoding 10)) 4]
               [(and (= platform 3) (= encoding 1)) 3]
               [(= platform 0) 2]
               [(and (= platform 3) (= encoding 0)) 1]
               [else 0]))
       (pick (+ i 8)
             (if (> rank best-rank) offset best)
             (if (> rank best-rank) rank best-rank))])))

;; format 4: BMP, segment mapping
(define (make-cmap4 data base)
  (define seg-count (quotient (u16 data (+ base 6)) 2))
  (define end-base (+ base 14))
  (define start-base (+ end-base (* 2 seg-count) 2))
  (define delta-base (+ start-base (* 2 seg-count)))
  (define range-base (+ delta-base (* 2 seg-count)))
  (lambda (char)
    (define c (char->integer char))
    (and (<= c #xFFFF)
         (let search ([i 0])
           (cond
             [(>= i seg-count) 0]
             [(<= (u16 data (+ end-base (* 2 i))) c) (search (add1 i))]
             [else
              (define start (u16 data (+ start-base (* 2 i))))
              (if (> start c)
                  0
                  (let ([range-off (u16 data (+ range-base (* 2 i)))])
                    (if (zero? range-off)
                        (bitwise-and #xFFFF (+ c (s16 data (+ delta-base (* 2 i)))))
                        (let ([g (u16 data (+ range-base (* 2 i) range-off (* 2 (- c start))))])
                          (if (zero? g)
                              0
                              (bitwise-and #xFFFF (+ g (s16 data (+ delta-base (* 2 i))))))))))])))))

;; format 12: 32-bit groups (linear scan; lookups are cached per glyph anyway)
(define (make-cmap12 data base)
  (define n (u32 data (+ base 12)))
  (lambda (char)
    (define c (char->integer char))
    (let search ([i 0])
      (cond
        [(>= i n) 0]
        [else
         (define gbase (+ base 16 (* 12 i)))
         (define start-c (u32 data gbase))
         (define end-c (u32 data (+ gbase 4)))
         (cond
           [(> c end-c) (search (add1 i))]
           [(< c start-c) 0]
           [else (+ (u32 data (+ gbase 8)) (- c start-c))])]))))

;; format 6: trimmed table (16-bit codes)
(define (make-cmap6 data base)
  (define first-code (u16 data (+ base 6)))
  (define count (u16 data (+ base 8)))
  (lambda (char)
    (define c (char->integer char))
    (define d (- c first-code))
    (if (and (>= d 0) (< d count))
        (u16 data (+ base 10 (* 2 d)))
        0)))

;; format 0: byte map
(define (make-cmap0 data base)
  (lambda (char)
    (define c (char->integer char))
    (if (< c 256) (u8 data (+ base 6 c)) 0)))

;; ---- kern --------------------------------------------------------------------------------

(define (parse-kern data tables)
  ;; TODO(0.2): the legacy 'kern' header differs between the Apple and
  ;; Microsoft variants and needs a dedicated validated pass; kerning is
  ;; disabled rather than half-correct.
  (define span (hash-ref tables "kern" #f))
  (if #t
      (hasheq)
      (let* ([base (car span)]
             [n-sub (u16 data (+ base 4))])
        ;; first subtable only; format 0 (horizontal pairs)
        (let* ([sub (+ base 6)]
               [version (u16 data sub)]
               [length (u16 data (+ sub 2))]
               [format (u16 data (+ sub 4))])
          (if (not (= format 0))
              (hasheq)
              (let* ([pairs (u16 data (+ sub 6))]
                     [entry (+ sub 14)]
                     [h (make-hash)])
                (for ([p (in-range pairs)])
                  (define off (+ entry (* 6 p)))
                  (define left (u16 data off))
                  (define right (u16 data (+ off 2)))
                  (hash-set! h (+ (* left 65536) right) (s16 data (+ off 4))))
                h))))))

;; ---- glyph outlines -------------------------------------------------------------------------

(struct outline-point (x y on-curve?) #:transparent)

;; Extract a glyph's contours in font units. Handles simple and composite
;; glyphs; composite component transforms are accumulated.
(define (glyph-outline f glyph-id [matrix #f] [dx 0] [dy 0])
  (define glyf (table-span f "glyf"))
  (unless glyf (error 'glyph-outline "font has no glyf table"))
  (define loca (table-subbytes f "loca"))
  (define base (car glyf))
  (define g-start
    (let ([off (if (= (font-index-to-loc-format f) 0)
                   (* 2 (u16 loca (* 2 glyph-id)))
                   (u32 loca (* 4 glyph-id)))])
      (+ base off)))
  (define g-end
    (let ([off (if (= (font-index-to-loc-format f) 0)
                   (* 2 (u16 loca (* 2 (add1 glyph-id))))
                   (u32 loca (* 4 (add1 glyph-id))))])
      (+ base off)))
  (when (>= g-start g-end)
    ;; empty glyph (space etc.)
    (apply-transform-result '() matrix dx dy))
  (define n-contours (s16 (font-data f) g-start))
  (if (>= n-contours 0)
      (collect-simple-glyph f g-start g-end n-contours matrix dx dy)
      (collect-composite-glyph f g-start matrix dx dy)))

;; Apply accumulated transform/offset to a font-unit point.
(define (xf matrix dx dy x y)
  (define tx (if matrix (+ (* (vector-ref matrix 0) x) (* (vector-ref matrix 2) y)) x))
  (define ty (if matrix (+ (* (vector-ref matrix 1) x) (* (vector-ref matrix 3) y)) y))
  (outline-point (+ tx dx) (+ ty dy) #t))

(define (apply-transform-result contours matrix dx dy)
  (if (and (not matrix) (zero? dx) (zero? dy))
      contours
      (for/list ([c (in-list contours)])
        (for/list ([p (in-list c)])
          (xf matrix dx dy (outline-point-x p) (outline-point-y p))))))

(define (collect-simple-glyph f start end n-contours matrix dx dy)
  (define d (font-data f))
  (define ends-base (+ start 10))
  (define last-end (add1 (u16 d (+ ends-base (* 2 (sub1 n-contours))))))
  (define instr-len (u16 d (+ ends-base (* 2 n-contours))))
  (define flags-base (+ ends-base (* 2 n-contours) 2 instr-len))

  ;; flags (with run-length repeats), tracking bytes consumed
  (define flags-vec (make-vector last-end 0))
  (define pos-box (box flags-base))
  (let fill ([i 0])
    (when (< i last-end)
      (define pos (unbox pos-box))
      (define flag (u8 d pos))
      (define rep (if (not (zero? (bitwise-and flag #x08))) (u8 d (add1 pos)) 0))
      (define end-i (min last-end (+ i 1 rep)))
      (for ([j (in-range i end-i)]) (vector-set! flags-vec j flag))
      (set-box! pos-box (+ pos (if (zero? rep) 1 2)))
      (fill end-i)))
  (define flags (lambda (i) (vector-ref flags-vec i)))

  ;; x coordinates: delta-coded per flag
  (define xs (make-vector last-end 0))
  (define x-end-pos
    (let walk ([i 0] [pos (unbox pos-box)] [x 0])
      (if (>= i last-end)
          pos
          (let* ([flag (flags i)]
                 [short? (not (zero? (bitwise-and flag #x02)))]
                 [same? (not (zero? (bitwise-and flag #x10)))]
                 [dx-val (cond
                           [short? (u8 d pos)]
                           [same? 0]
                           [else (s16 d pos)])]
                 [width (cond [short? 1] [same? 0] [else 2])]
                 [signed-dx (if (and short? (not same?)) (- dx-val) dx-val)])
            (vector-set! xs i (+ x signed-dx))
            (walk (add1 i) (+ pos width) (+ x signed-dx))))))

  ;; y coordinates continue from where the x walk stopped
  (define ys (make-vector last-end 0))
  (let walk ([i 0] [pos x-end-pos] [y 0])
    (when (< i last-end)
      (let* ([flag (flags i)]
             [short? (not (zero? (bitwise-and flag #x04)))]
             [same? (not (zero? (bitwise-and flag #x20)))]
             [dy-val (cond
                       [short? (u8 d pos)]
                       [same? 0]
                       [else (s16 d pos)])]
             [width (cond [short? 1] [same? 0] [else 2])]
             [signed-dy (if (and short? (not same?)) (- dy-val) dy-val)])
        (vector-set! ys i (+ y signed-dy))
        (walk (add1 i) (+ pos width) (+ y signed-dy)))))

  ;; split into contours by end points
  (define contours
    (let split ([i 0] [contour-idx 0] [acc '()] [accs '()])
      (cond
        [(>= i last-end)
         (reverse (if (null? acc) accs (cons (reverse acc) accs)))]
        [else
         (define end-pt (u16 d (+ ends-base (* 2 contour-idx))))
         (define p (outline-point (vector-ref xs i)
                                  (vector-ref ys i)
                                  (not (zero? (bitwise-and (flags i) #x01)))))
         (if (= i end-pt)
             (split (add1 i) (add1 contour-idx) '() (cons (reverse (cons p acc)) accs))
             (split (add1 i) contour-idx (cons p acc) accs))])))
  (apply-transform-result contours matrix dx dy))

;; Drop points at the exact composite transform site for empty glyphs.
(define (collect-composite-glyph f start matrix dx dy)
  (define d (font-data f))
  (define pos (+ start 10))
  (define contours '())
  (let components ()
    (define flags (u16 d pos))
    (define glyph-index (u16 d (+ pos 2)))
    (set! pos (+ pos 4))
    (define arg1 (if (not (zero? (bitwise-and flags #x0001)))
                     (begin0 (s16 d pos) (set! pos (+ pos 2)))
                     (begin0 (s8 d pos) (set! pos (add1 pos)))))
    (define arg2 (if (not (zero? (bitwise-and flags #x0001)))
                     (begin0 (s16 d pos) (set! pos (+ pos 2)))
                     (begin0 (s8 d pos) (set! pos (add1 pos)))))
    ;; only XY-VALUE args supported (point-matching is vanishingly rare)
    (define cdx (if (not (zero? (bitwise-and flags #x0002))) arg1 0))
    (define cdy (if (not (zero? (bitwise-and flags #x0002))) arg2 0))
    (define m
      (cond
        [(not (zero? (bitwise-and flags #x0080)))
         ;; 2x2: xx xy yx yy (F2Dot14 each)
         (begin0 (vector (/ (s16 d pos) 16384.0)
                         (/ (s16 d (+ pos 2)) 16384.0)
                         (/ (s16 d (+ pos 4)) 16384.0)
                         (/ (s16 d (+ pos 6)) 16384.0))
                 (set! pos (+ pos 8)))]
        [(not (zero? (bitwise-and flags #x0040)))
         (define xs (/ (s16 d pos) 16384.0))
         (define ys (/ (s16 d (+ pos 2)) 16384.0))
         (set! pos (+ pos 4))
         (vector xs 0 0 ys)]
        [(not (zero? (bitwise-and flags #x0008)))
         (define s (/ (s16 d pos) 16384.0))
         (set! pos (+ pos 2))
         (vector s 0 0 s)]
        [else (vector 1 0 0 1)]))
    ;; accumulate: child offset passes through the OUTER transform
    (define m2 (compose-matrix matrix m))
    (define odx (+ dx (if matrix (+ (* (vector-ref matrix 0) cdx) (* (vector-ref matrix 2) cdy)) cdx)))
    (define ody (+ dy (if matrix (+ (* (vector-ref matrix 1) cdx) (* (vector-ref matrix 3) cdy)) cdy)))
    (define child (glyph-outline f glyph-index m2 odx ody))
    (set! contours (append contours child))
    (when (not (zero? (bitwise-and flags #x0020))) (components)))
  contours)

;; 2x2 matrix compose: (a b / c d) given as vector(a b c d); result = outer ∘ m
(define (compose-matrix outer m)
  (if (not outer)
      m
      (vector (+ (* (vector-ref outer 0) (vector-ref m 0))
                 (* (vector-ref outer 2) (vector-ref m 1)))
              (+ (* (vector-ref outer 1) (vector-ref m 0))
                 (* (vector-ref outer 3) (vector-ref m 1)))
              (+ (* (vector-ref outer 0) (vector-ref m 2))
                 (* (vector-ref outer 2) (vector-ref m 3)))
              (+ (* (vector-ref outer 1) (vector-ref m 2))
                 (* (vector-ref outer 3) (vector-ref m 3))))))

;; note: collect-simple-glyph returns transformed contours already; the
;; composite path appends transformed children. The early-empty helper:
(define (return-contours contours matrix dx dy)
  (apply-transform-result contours matrix dx dy))

;; ---- metrics ---------------------------------------------------------------------------------

(define (glyph-advance f glyph-id)
  (define hmtx (table-subbytes f "hmtx"))
  (unless hmtx (error 'glyph-advance "font has no hmtx"))
  (define n (font-num-h-metrics f))
  (if (< glyph-id n)
      (u16 hmtx (* 4 glyph-id))
      (u16 hmtx (* 4 (sub1 n)))))

(define (glyph-kern f left right)
  (hash-ref (font-kerning f) (+ (* left 65536) right) 0))

(define (font-ascent f) (font-ascender f))
(define (font-descent f) (font-descender f))

(define (font-line-height f)
  (+ (font-ascender f) (- (font-descender f)) (font-line-gap f)))

(define (font-units->px f units pixel-size)
  (/ (* units pixel-size 1.0) (font-units-per-em f)))

;; ---- loading -----------------------------------------------------------------------------------

(define (load-font path [index 0])
  (define raw (file->bytes path))
  (define-values (data base tables)
    (if (and (> (bytes-length raw) 4) (equal? (subbytes raw 0 4) #"ttcf"))
        ;; TrueType collection: jump to the requested font
        (let* ([n (u32 raw 8)]
               [off (u32 raw (+ 12 (* 4 (min index (sub1 n)))))])
          (values raw off (parse-table-directory raw off)))
        (values raw 0 (parse-table-directory raw 0))))
  (define head (hash-ref tables "head" #f))
  (define maxp (hash-ref tables "maxp" #f))
  (define hhea (hash-ref tables "hhea" #f))
  (unless (and head maxp hhea)
    (error 'load-font "not a TrueType font (missing head/maxp/hhea): ~a" path))
  (unless (hash-ref tables "glyf" #f)
    (error 'load-font "CFF/PostScript outlines not supported (no glyf table): ~a" path))
  (define units-per-em (u16 data (+ (car head) 18)))
  (define index-to-loc-format (s16 data (+ (car head) 50)))
  (define num-glyphs (u16 data (+ (car maxp) 4)))
  (define ascender (s16 data (+ (car hhea) 4)))
  (define descender (s16 data (+ (car hhea) 6)))
  (define line-gap (s16 data (+ (car hhea) 8)))
  (define num-h-metrics (u16 data (+ (car hhea) 34)))
  (font data tables units-per-em num-glyphs index-to-loc-format
        ascender descender line-gap num-h-metrics
        (parse-cmap data tables)
        (parse-kern data tables)))

