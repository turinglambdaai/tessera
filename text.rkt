#lang racket/base

;; Text for tessera: glyph rasterization (scanline area accumulation),
;; a shared glyph atlas, and line layout (advances, kerning, wrapping).
;;
;; Glyphs are rasterized on demand at a fixed pixel size and cached in a
;; texture atlas keyed by codepoint. The rasterizer sub-samples each pixel
;; row 4x and accumulates exact span coverage, which gives clean grayscale
;; anti-aliasing for both Latin and CJK glyphs without any hinting.

(require racket/bytes
         racket/file
         racket/list
         racket/format
         racket/list
         racket/math
         racket/string
         tessera/font
         tessera/ffi/gl
         tessera/render)

(provide (struct-out cell)
         (struct-out font-set)
         (struct-out atlas)
         rasterize-glyph
         x->caret
         find-font-file
         make-font-set
         font-set-texture
         text-width
         wrap-text
         draw-text!
         glyph-cell-lookup)

;; ---- platform font resolution -----------------------------------------------------

(define font-candidates
  (case (system-type 'os)
    [(macosx)
     (list "/System/Library/Fonts/Helvetica.ttc"
           "/System/Library/Fonts/HelveticaNeue.ttc"
           "/System/Library/Fonts/Menlo.ttc"
           "/System/Library/Fonts/Supplemental/Arial.ttf"
           "/System/Library/Fonts/SFNS.ttf"
           "/System/Library/Fonts/STHeiti Medium.ttc"
           "/System/Library/Fonts/Supplemental/Songti.ttc"
           "/Library/Fonts/Arial.ttf")]
    [(unix)
     (list "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
           "/usr/share/fonts/TTF/DejaVuSans.ttf"
           "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
           ;; WenQuanYi Zen Hei is a TrueType collection and therefore works
           ;; with tessera's current pure-Racket glyf outline parser. Ubuntu
           ;; and Debian provide this path via fonts-wqy-zenhei.
           "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc"
           "/usr/share/fonts/wqy-zenhei/wqy-zenhei.ttc"
           ;; Keep common Noto locations for future/locally-installed TTF
           ;; variants. Many distro Noto CJK packages are OpenType/CFF and are
           ;; intentionally rejected by the current parser.
           "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc"
           "/usr/share/fonts/noto-cjk/NotoSansCJK-Regular.ttc")]
    [(windows)
     (list "C:/Windows/Fonts/segoeui.ttf"
           "C:/Windows/Fonts/arial.ttf"
           "C:/Windows/Fonts/msyh.ttc")]
    [else '()]))

;; First candidate that parses as a TrueType font. `#:require-glyph` asks
;; that the font actually contain the given character (used to pick a CJK-
;; capable face when needed).
(define (find-font-file #:require-glyph [require-glyph #f])
  (define from-env (getenv "TESSERA_FONT"))
  (define candidates (if from-env (cons from-env font-candidates) font-candidates))
  (or (for/or ([path (in-list candidates)])
        (and path
             (file-exists? path)
             (with-handlers ([exn:fail? (λ (_) #f)])
               (define f (load-font path))
               (if (and require-glyph
                        (zero? ((font-cmap-lookup f) require-glyph)))
                   #f
                   path))))
      (error 'tessera/text
             "no usable TrueType font found; point TESSERA_FONT at a .ttf/.ttc file")))

;; ---- structures ----------------------------------------------------------------------

(define atlas-size 2048)
(define atlas-pad 2)          ; zeroed border around each glyph (bleed guard)

;; Glyph placement inside the atlas, normalized coords for rendering.
(struct cell (u0 v0 u1 v1             ; atlas UV rect (inset by the pad)
              w h                     ; bitmap size, device px
              bearing-x bearing-y     ; offset from origin, device px (y-down)
              advance)                ; pen advance, device px
  #:transparent)

;; Mutable atlas: simple row packing. The texture is created lazily on the
;; first rasterization, so pure metric queries work without a GL context.
(struct atlas ([tex #:mutable]
               [cursor-x #:mutable]
               [cursor-y #:mutable]
               [row-h #:mutable])
  #:transparent)

;; A font-set = parsed font + pixel size + rasterization cache.
(struct font-set (font px-size atlas glyphs) #:transparent) ; glyphs: hash char -> cell

(define (make-font-set path px-size)
  (font-set (load-font path) px-size
            (atlas #f 1 1 0)
            (make-hasheq)))

;; Ensure the atlas texture exists (needs a current GL context).
(define (atlas-ensure-texture! a)
  (unless (atlas-tex a)
    (set-atlas-tex! a (renderer-texture-alpha atlas-size atlas-size #f)))
  (atlas-tex a))

(define (font-set-texture fs)
  (atlas-ensure-texture! (font-set-atlas fs)))

(define (glyph-cell-lookup fs char)
  (glyph-cell fs char))

;; ---- contour flattening -----------------------------------------------------------------

;; TrueType quadratic segment sampling positions (4 segments per quad).
(define quad-t '(0.25 0.5 0.75 1.0))

;; Append to `acc` the points at t = .25/.5/.75/1 of quad (p0 ctrl p1).
;; All three are (x . y) pairs.
(define (emit-quad acc p0 ctrl p1)
  (for/fold ([acc acc])
            ([t (in-list quad-t)])
    (define mt (- 1.0 t))
    (cons (cons (+ (* mt mt (car p0)) (* 2.0 mt t (car ctrl)) (* t t (car p1)))
                (+ (* mt mt (cdr p0)) (* 2.0 mt t (cdr ctrl)) (* t t (cdr p1))))
          acc)))

;; Convert a contour (list of outline-point) into a closed polyline of
;; (x . y) pairs, resolving on/off-curve runs into subdivided quadratics.
(define (fix-contour c)
  ;; Convert one contour of outline-points into a closed polyline of (x . y).
  ;; TrueType on/off-curve rules: an off-curve point between two on-curve
  ;; points is a quadratic control; two consecutive off-curve points imply
  ;; an on-curve midpoint between them.
  (define n (length c))
  (when (< n 2)
    ;; single-point contours (i/j dots) vanish at raster sizes
    (raise 'degenerate-contour #f))
  (define (ref i) (list-ref c (modulo i n)))
  (define (on? i) (outline-point-on-curve? (ref i)))
  (define (xy i) (cons (outline-point-x (ref i)) (outline-point-y (ref i))))
  (define (mid a b)
    (cons (/ (+ (car a) (car b)) 2.0) (/ (+ (cdr a) (cdr b)) 2.0)))

  (define result '())            ; reversed polyline
  (define (emit p) (set! result (cons p result)))

  (define (emit-quad-pts p0 ctrl p1)
    (for ([t (in-list quad-t)])
      (define mt (- 1.0 t))
      (emit (cons (+ (* mt mt (car p0)) (* 2.0 mt t (car ctrl)) (* t t (car p1)))
                  (+ (* mt mt (cdr p0)) (* 2.0 mt t (cdr ctrl)) (* t t (cdr p1)))))))

  (define first-on
    (for/first ([i (in-range n)] #:when (on? i)) i))

  (cond
    ;; no on-curve points: everything is implied midpoints
    [(not first-on)
     (define p0 (mid (xy 0) (xy 1)))
     (emit p0)
     (let walk ([i 1] [prev p0])
       (when (< i n)
         (define ctrl (xy i))
         (define nxt (mid (xy i) (xy (modulo (add1 i) n))))
         (emit-quad-pts prev ctrl nxt)
         (walk (add1 i) nxt)))]

    ;; normal contour: start at an on-curve point, walk the full cycle
    [else
     (emit (xy first-on))
     (let walk ([i first-on])
       (define j (modulo (add1 i) n))
       (cond
         [(on? j)
          (emit (xy j))
          (unless (= j first-on) (walk j))]
         [else
          (define k (modulo (add1 j) n))
          (cond
            [(on? k)
             (emit-quad-pts (xy i) (xy j) (xy k))
             (unless (= k first-on) (walk k))]
            [else
             ;; implied on-curve midpoint between two controls
             (define m (mid (xy j) (xy k)))
             (emit-quad-pts (xy i) (xy j) m)
             ;; continue from implied point. The remaining original off-curve
             ;; point k acts as the next control.
             (let walk-off ([prev m] [ctrl-index k])
               (define next-index (modulo (add1 ctrl-index) n))
               (cond
                 [(on? next-index)
                  (emit-quad-pts prev (xy ctrl-index) (xy next-index))
                  (unless (= next-index first-on) (walk next-index))]
                 [else
                  (define m2 (mid (xy ctrl-index) (xy next-index)))
                  (emit-quad-pts prev (xy ctrl-index) m2)
                  (walk-off m2 next-index)]))])]))])

  (reverse result))

;; Flatten all glyph contours, dropping degenerate ones.
(define (flatten-outline contours)
  (for/list ([c (in-list contours)]
             #:do [(define fixed
                     (with-handlers ([(λ (e) (eq? e 'degenerate-contour))
                                      (λ (_) #f)])
                       (fix-contour c)))]
             #:when fixed)
    fixed))

;; ---- scanline rasterizer -----------------------------------------------------------

(define samples-y 4)

;; Intersections between a polyline and a horizontal scanline y.
(define (scan-intersections poly y)
  (define v (list->vector poly))
  (define n (vector-length v))
  (sort
   (for/list ([i (in-range n)]
              #:do [(define a (vector-ref v i))
                    (define b (vector-ref v (modulo (add1 i) n)))]
              #:when (or (and (<= (cdr a) y) (> (cdr b) y))
                         (and (<= (cdr b) y) (> (cdr a) y))))
     (+ (car a)
        (* (/ (- y (cdr a)) (- (cdr b) (cdr a)))
           (- (car b) (car a)))))
   <))

;; Rasterize one glyph to an 8-bit alpha bitmap and metrics.
;; Returns values: bytes w h bearing-x bearing-y advance.
(define (rasterize-glyph f char px-size)
  (define glyph-index ((font-cmap-lookup f) char))
  (define advance (exact-round (font-units->px f (glyph-advance f glyph-index) px-size)))
  (define contours (glyph-outline f glyph-index))
  (cond
    [(null? contours)
     (values #"" 0 0 0 0 advance)]
    [else
     (define polys (flatten-outline contours))
     (if (null? polys)
         (values #"" 0 0 0 0 advance)
         (let* ([s (/ px-size (font-units-per-em f))]
                [all (apply append polys)]
                [xs (map (λ (p) (* s (car p))) all)]
                ;; flip font y-up into bitmap y-down
                [ys (map (λ (p) (* -1.0 s (cdr p))) all)]
                [min-x (floor (apply min xs))]
                [max-x (ceiling (apply max xs))]
                [min-y (floor (apply min ys))]
                [max-y (ceiling (apply max ys))]
                [w (max 0 (exact-round (- max-x min-x)))]
                [h (max 0 (exact-round (- max-y min-y)))]
                [scaled-polys
                 (for/list ([poly (in-list polys)])
                   (for/list ([p (in-list poly)])
                     (cons (- (* s (car p)) min-x)
                           (- (* -1.0 s (cdr p)) min-y))))]
                [bitmap (make-bytes (* w h) 0)])
           (for ([py (in-range h)])
             (for ([sy (in-range samples-y)])
               (define y (+ py (/ (+ sy 0.5) samples-y)))
               ;; non-zero fill via even/odd spans per contour; overlapping
               ;; contours are OR-combined into coverage.
               (define row-coverage (make-vector w 0))
               (for ([poly (in-list scaled-polys)])
                 (define xs* (scan-intersections poly y))
                 (let spans ([rest xs*])
                   (when (>= (length rest) 2)
                     (define x0 (first rest))
                     (define x1 (second rest))
                     (define lo (max 0 (inexact->exact (floor x0))))
                     (define hi (min w (inexact->exact (ceiling x1))))
                     (for ([px (in-range lo hi)])
                       (define cov (max 0.0 (- (min (+ px 1.0) x1)
                                              (max (exact->inexact px) x0))))
                       (when (> cov 0)
                         (vector-set! row-coverage px
                                      (+ (vector-ref row-coverage px) cov))))
                     (spans (cddr rest)))))
               (for ([px (in-range w)])
                 (define cov (min 1.0 (/ (vector-ref row-coverage px) samples-y)))
                 (when (> cov 0)
                   (bytes-set! bitmap (+ (* py w) px)
                               (min 255 (+ (bytes-ref bitmap (+ (* py w) px))
                                           (exact-round (* 255 cov)))))))))
           (values bitmap w h (exact-round min-x) (exact-round min-y) advance)))]))

;; ---- atlas packing -------------------------------------------------------------------

(define (glyph-cell fs char)
  (hash-ref!
   (font-set-glyphs fs) char
   (λ ()
     (define-values (bmp w h bx by adv)
       (rasterize-glyph (font-set-font fs) char (font-set-px-size fs)))
     (cond
       [(or (= w 0) (= h 0))
        (cell 0.0 0.0 0.0 0.0 0 0 bx by adv)]
       [else
        (define a (font-set-atlas fs))
        (define tex (atlas-ensure-texture! a))
        (define packed-w (+ w (* 2 atlas-pad)))
        (define packed-h (+ h (* 2 atlas-pad)))
        (when (> (+ (atlas-cursor-x a) packed-w) atlas-size)
          (set-atlas-cursor-x! a 1)
          (set-atlas-cursor-y! a (+ (atlas-cursor-y a) (atlas-row-h a)))
          (set-atlas-row-h! a 0))
        (when (> (+ (atlas-cursor-y a) packed-h) atlas-size)
          (error 'tessera/text "glyph atlas full"))
        (define x (+ (atlas-cursor-x a) atlas-pad))
        (define y (+ (atlas-cursor-y a) atlas-pad))
        (glBindTexture GL_TEXTURE_2D tex)
        (glPixelStorei GL_UNPACK_ALIGNMENT 1)
        (glTexSubImage2D GL_TEXTURE_2D 0 x y w h GL_ALPHA GL_UNSIGNED_BYTE bmp)
        (glBindTexture GL_TEXTURE_2D 0)
        (set-atlas-cursor-x! a (+ (atlas-cursor-x a) packed-w))
        (set-atlas-row-h! a (max (atlas-row-h a) packed-h))
        (cell (/ x atlas-size) (/ y atlas-size)
              (/ (+ x w) atlas-size) (/ (+ y h) atlas-size)
              w h bx by adv)]))))

(define (text-width fs str)
  (for/fold ([x 0] [prev #f] #:result x)
            ([ch (in-string str)])
    (define kern (if prev (glyph-kern (font-set-font fs)
                                      ((font-cmap-lookup (font-set-font fs)) prev)
                                      ((font-cmap-lookup (font-set-font fs)) ch))
                     0))
    (values (+ x (font-units->px (font-set-font fs) kern (font-set-px-size fs))
               (cell-advance (glyph-cell fs ch)))
            ch)))

;; Caret index nearest a horizontal position in device pixels.
(define (x->caret fs str x)
  (let loop ([chars (string->list str)] [i 0] [pen 0.0])
    (cond
      [(null? chars) i]
      [else
       (define adv (cell-advance (glyph-cell fs (car chars))))
       (if (< x (+ pen (/ adv 2.0))) i
           (loop (cdr chars) (add1 i) (+ pen adv)))])))

(define (wrap-text fs str max-width)
  ;; Greedy word wrap. For CJK/non-space runs, falls back to char boundaries.
  (define words (regexp-split #px"(?<=\\s)|(?=\\s)" str))
  (define lines '())
  (define current "")
  (define (push!)
    (unless (string=? current "")
      (set! lines (cons (string-trim current) lines))
      (set! current "")))
  (for ([word (in-list words)])
    (define candidate (string-append current word))
    (cond
      [(<= (text-width fs candidate) max-width)
       (set! current candidate)]
      [(string=? current "")
       ;; one token is too wide: split at characters
       (for ([ch (in-string word)])
         (define c (string ch))
         (if (<= (text-width fs (string-append current c)) max-width)
             (set! current (string-append current c))
             (begin (push!) (set! current c))))]
      [else
       (push!)
       (set! current word)]))
  (push!)
  (reverse lines))

;; Draw a string using one font-set. x/y are logical points; glyph bitmaps are
;; device-pixel-sized, so position is scaled but cell metrics are already px.
(define (draw-text! r fs str x y c)
  (define s (renderer-scale r))
  (define tex (font-set-texture fs))
  (r-use-texture! r tex)
  (define pen-x (* x s))
  (define baseline (+ (* y s) (* (font-ascent (font-set-font fs))
                                  (/ (font-set-px-size fs)
                                     (font-units-per-em (font-set-font fs))))))
  (define prev-gid #f)
  (for ([ch (in-string str)])
    (define gid ((font-cmap-lookup (font-set-font fs)) ch))
    (when prev-gid
      (set! pen-x (+ pen-x
                     (font-units->px (font-set-font fs)
                                     (glyph-kern (font-set-font fs) prev-gid gid)
                                     (font-set-px-size fs)))))
    (define g (glyph-cell fs ch))
    (when (and (> (cell-w g) 0) (> (cell-h g) 0))
      (r-quad-uv-raw! r
                      (+ pen-x (cell-bearing-x g))
                      (+ baseline (cell-bearing-y g))
                      (cell-w g) (cell-h g)
                      (cell-u0 g) (cell-v0 g) (cell-u1 g) (cell-v1 g) c))
    (set! pen-x (+ pen-x (cell-advance g)))
    (set! prev-gid gid))
  (r-use-texture! r #f)
  (/ pen-x s))
