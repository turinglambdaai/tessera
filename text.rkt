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
          ;; collect the off-curve chain until the next on-curve point
          (let chain ([offs (list (xy j))] [k (modulo (add1 j) n)])
            (cond
              [(on? k)
               ;; chained quads: p -> o1 -> m(o1,o2) -> o2 -> ... -> (xy k)
               (let quads ([prev (car result)] [offs2 offs])
                 (cond
                   [(null? offs2) (void)]
                   [(null? (cdr offs2))
                    (emit-quad-pts prev (car offs2) (xy k))]
                   [else
                    (define m (mid (car offs2) (cadr offs2)))
                    (emit-quad-pts prev (car offs2) m)
                    (quads m (cdr offs2))]))
               (emit (xy k))
               (unless (= k first-on) (walk k))]
              [else
               (chain (append offs (list (xy k))) (modulo (add1 k) n))]))]))])
  (reverse result))

;; Flatten all contours to device-space edges (y flipped for screen space).
(define (flatten-edges f glyph-id px-size)
  (define scale (/ px-size 1.0 (font-units-per-em f)))
  (define (px x) (* x scale))
  (define (py y) (* (- y) scale))
  (append*
   (for/list ([c (in-list (glyph-outline f glyph-id))])
     (define pts
       (with-handlers ([symbol? (λ (sym) (if (eq? sym 'degenerate-contour) '() (raise sym #f)))])
         (fix-contour c)))
     (if (< (length pts) 2)
         '()
         (for/list ([i (in-range (length pts))])
           (define p0 (list-ref pts i))
           (define p1 (list-ref pts (modulo (add1 i) (length pts))))
           (cons (cons (px (car p0)) (py (cdr p0)))
                 (cons (px (car p1)) (py (cdr p1)))))))))

;; ---- scanline rasterizer ------------------------------------------------------------------

;; edges: list of ((x0 . y0) x1 . y1)? — no: list of ((x0.y0) . ((x1.y1)))
;; from flatten-edges. Normalize to flat (x0 y0 x1 y1) lists here.
(define (rasterize-glyph fs char)
  (define f (font-set-font fs))
  (define s (font-set-px-size fs))
  (define gid ((font-cmap-lookup f) char))
  (define advance (exact-round (font-units->px f (glyph-advance f gid) s)))
  (if (zero? gid)
      (values #"" 0 0 0 0 advance)
      (let* ([flat (flatten-edges f gid s)]
             [edges (for/list ([e (in-list flat)])
                      (vector (car (car e)) (cdr (car e))
                              (car (cdr e)) (cdr (cdr e))))]
             [xmins (for/list ([e (in-list edges)]) (min (vector-ref e 0) (vector-ref e 2)))]
             [xmaxs (for/list ([e (in-list edges)]) (max (vector-ref e 0) (vector-ref e 2)))]
             [ymins (for/list ([e (in-list edges)]) (min (vector-ref e 1) (vector-ref e 3)))]
             [ymaxs (for/list ([e (in-list edges)]) (max (vector-ref e 1) (vector-ref e 3)))])
        (if (null? edges)
            (values #"" 0 0 0 0 advance)
            (let* ([xmin (exact-floor (- (foldl min +inf.0 xmins) 0.5))]
                   [ymin (exact-floor (- (foldl min +inf.0 ymins) 0.5))]
                   [xmax (exact-ceiling (+ (foldl max -inf.0 xmaxs) 0.5))]
                   [ymax (exact-ceiling (+ (foldl max -inf.0 ymaxs) 0.5))]
                   [w (max 1 (- xmax xmin))]
                   [h (max 1 (- ymax ymin))]
                   [cov (rasterize-edges edges xmin ymin w h)]
                   [bearing-x xmin]
                   [bearing-y ymin])
              (values cov w h bearing-x bearing-y advance))))))

(define (rasterize-edges edges xmin ymin w h)
  (define cov (make-bytes (* w h)))
  (define row-cov (make-vector w 0.0))
  (define subs 4)
  (define inv-sub (/ 1.0 subs))
  (for ([row (in-range h)])
    (vector-fill! row-cov 0.0)
    (for ([k (in-range subs)])
      (define y (+ ymin row (* inv-sub (+ k 0.5))))
      ;; collect crossings of this sub-row, as (x . winding-direction)
      (define xs
        (for/list ([e (in-list edges)]
                   #:when (let ([ey0 (vector-ref e 1)] [ey1 (vector-ref e 3)])
                            (and (not (= ey0 ey1))
                                 (>= y (min ey0 ey1))
                                 (< y (max ey0 ey1)))))
          (define ex0 (vector-ref e 0))
          (define ey0 (vector-ref e 1))
          (define ex1 (vector-ref e 2))
          (define ey1 (vector-ref e 3))
          (define t (/ (- y ey0) (- ey1 ey0)))
          (cons (+ ex0 (* t (- ex1 ex0)))
                (if (> ey1 ey0) 1 -1))))
      ;; nonzero winding walk
      (define sorted (sort xs < #:key car))
      (define count (length sorted))
      (let walk ([i 0] [winding 0] [span-start 0.0])
        (when (< i count)
          (define xc (car (list-ref sorted i)))
          (define dir (cdr (list-ref sorted i)))
          (define nw (+ winding dir))
          (cond
            [(zero? winding)
             (walk (add1 i) nw xc)]
            [(zero? nw)
             ;; close span [span-start, xc)
             (define px-start (max 0 (exact-floor (- span-start xmin))))
             (define px-end (min w (exact-ceiling (- xc xmin))))
             (for ([px (in-range px-start px-end)])
               (define lo (max span-start (+ xmin px)))
               (define hi (min xc (+ xmin px 1.0)))
               (when (< lo hi)
                 (vector-set! row-cov px
                              (+ (vector-ref row-cov px) (* (- hi lo) inv-sub)))))
             (walk (add1 i) nw span-start)]
            [else
             (walk (add1 i) nw span-start)]))))
    (for ([col (in-range w)])
      (define v (vector-ref row-cov col))
      (unless (zero? v)
        (bytes-set! cov (+ (* row w) col)
                    (min 255 (exact-round (* 255.0 v)))))))
  cov)

;; ---- atlas placement ------------------------------------------------------------------------

(define (glyph-cell fs char)
  (define glyphs (font-set-glyphs fs))
  (or (hash-ref glyphs char #f)
      (let-values ([(bytes w h bx by adv) (rasterize-glyph fs char)])
        (define a (font-set-atlas fs))
        (define cell*
          (if (or (zero? w) (zero? h))
              (cell 0.0 0.0 0.0 0.0 0 0 0 0 adv)
              (let* ([nx (atlas-cursor-x a)]
                     [ny (atlas-cursor-y a)]
                     [padded bytes])
                (when (> (+ ny h) (sub1 atlas-size))
                  (error 'tessera/text "glyph atlas exhausted (2048px); file an issue with your font"))
                (glBindTexture GL_TEXTURE_2D (atlas-ensure-texture! a))
                (glPixelStorei GL_UNPACK_ALIGNMENT 1)
                (glTexSubImage2D GL_TEXTURE_2D 0 nx ny w h
                                 GL_ALPHA GL_UNSIGNED_BYTE padded)
                (let ([err (glGetError)])
                  (unless (zero? err)
                    (eprintf "TexSub FAILED for ~a: err ~a (w=~a h=~a)
" char err w h)))
                (set-atlas-cursor-x! a (+ nx w 2))
                (set-atlas-cursor-y! a ny)
                (set-atlas-row-h! a (max (atlas-row-h a) h))
                (cell (/ nx atlas-size)
                      (/ ny atlas-size)
                      (/ (+ nx w) atlas-size)
                      (/ (+ ny h) atlas-size)
                      w h bx by adv))))
        (hash-set! glyphs char cell*)
        cell*)))

;; ---- layout ------------------------------------------------------------------------------------

;; Advance width of a string, device px at the font-set's size.
(define (text-width fs str)
  (define f (font-set-font fs))
  (define s (font-set-px-size fs))
  (let loop ([chars (string->list str)] [prev-gid #f] [x 0.0])
    (cond
      [(null? chars) (exact-round x)]
      [else
       (define gid ((font-cmap-lookup f) (car chars)))
       (define kern (if prev-gid (font-units->px f (glyph-kern f prev-gid gid) s) 0))
       (define adv (font-units->px f (glyph-advance f gid) s))
       (loop (cdr chars) gid (+ x kern adv))])))

;; Greedy wrapping; breaks allowed after spaces and between CJK glyphs.
(define (wrap-text fs str max-width)
  (define (breakable? c)
    (or (char-whitespace? c)
        (let ([code (char->integer c)])
          (or (and (>= code #x2E80) (<= code #x9FFF))
              (and (>= code #xF900) (<= code #xFAFF))
              (and (>= code #xFF00) (<= code #xFFEF))
              (>= code #x20000)))))
  (define chars (string->list str))
  (let wrap ([i 0] [line-start 0] [lines '()])
    (cond
      [(>= i (length chars))
       (reverse (cons (substring str line-start i) lines))]
      [else
       (define candidate (substring str line-start (add1 i)))
       (cond
         [(<= (text-width fs candidate) (max 1 max-width))
          (wrap (add1 i) line-start lines)]
         [(= i line-start)
          ;; single overlong unit: emit it anyway
          (wrap (add1 i) (add1 i) (cons candidate lines))]
         [else
          ;; find the last break opportunity within [line-start, i)
          (define break-at
            (or (for/last ([k (in-range line-start i)]
                           #:when (breakable? (list-ref chars k)))
                  (add1 k))
                i))
          (wrap break-at break-at (cons (substring str line-start break-at) lines))])])))

;; ---- drawing -------------------------------------------------------------------------------------

;; Draw a string with (x, y) at the top-left of the line box, in POINTS.
;; Glyph metrics are device px (the font-set rasterizes at px-size device
;; pixels), so the origin is converted once and the glyph stream runs in
;; raw device coordinates.
(define (draw-text! r fs str x y color)
  (unless (string=? str "")
    (r-use-texture! r (font-set-texture fs))
    (define f (font-set-font fs))
    (define s (font-set-px-size fs))
    (define ascent (font-units->px f (font-ascent f) s))
    (define x0 (* 1.0 x (renderer-scale r)))
    (define y0 (* 1.0 y (renderer-scale r)))
    (let draw ([chars (string->list str)] [prev-gid #f] [cx 0.0])
      (unless (null? chars)
        (define gid ((font-cmap-lookup f) (car chars)))
        (define kern (if prev-gid (font-units->px f (glyph-kern f prev-gid gid) s) 0))
        (define cell* (glyph-cell fs (car chars)))
        (when (> (cell-w cell*) 0)
          (r-quad-uv-raw! r
                          (+ x0 cx kern (cell-bearing-x cell*))
                          (+ y0 ascent (cell-bearing-y cell*))
                          (cell-w cell*) (cell-h cell*)
                          (cell-u0 cell*) (cell-v0 cell*)
                          (cell-u1 cell*) (cell-v1 cell*)
                          color))
        (define adv (font-units->px f (glyph-advance f gid) s))
        (draw (cdr chars) gid (+ cx kern adv))))))
