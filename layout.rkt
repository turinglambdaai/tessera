#lang racket/base

;; Layout and drawing for tessera view trees.
;;
;; `layout-view` measures and arranges a view tree, producing a `laid` tree:
;; every node annotated with a rectangle in POINTS and a path (list of
;; child indexes, stable across frames while the tree shape is stable).
;; `draw-laid!` lowers the laid tree to renderer calls; `hit-test` and
;; `path->laid` support event routing and focus tracking.
;;
;; Layout units are points; the renderer's frame scale converts to device
;; pixels at draw time. Fonts are keyed by point size through ui-ctx.
;;
;; Layout model (all containers):
;;   main axis  = row -> x, column -> y; children stack with `spacing` gaps
;;   cross axis = row -> y, column -> x
;;   flex > 0 children share leftover main-axis space proportionally
;;   align      = cross-axis placement: start | center | end | stretch

(require racket/list
         racket/match
         racket/promise
         racket/math
         racket/string
         tessera/render
         tessera/text
         tessera/theme
         tessera/view
         tessera/image)

(provide (struct-out laid)
         (struct-out ui-ctx)
         make-ui-ctx
         ui-ctx-font
         layout-view
         draw-laid!
         hit-test
         hit-interactive
         in-rect?
         path->laid
         current-hover-path
         current-focus-path)

;; ---- context ---------------------------------------------------------------------

;; fonts: procedure pt-size -> (cons primary-font-set fallback-or-#f),
;; memoized per size by the maker. The fallback serves glyphs the primary
;; face lacks (CJK on a Latin-primary stack).
(struct ui-ctx (fonts scale
                     scroll-offsets   ; hash: path -> px offset (mutable, run-owned)
                     images)          ; hash: src -> (list w h texture-id)
  #:transparent)

(define (make-ui-ctx font-make fallback-make scale
                     [scroll-offsets (make-hash)] [images (make-hash)])
  (define cache (make-hash))
  (define (fonts pt)
    (hash-ref! cache pt
               (λ () (define fb (and fallback-make (fallback-make pt)))
                  (cons (font-make pt) fb))))
  (ui-ctx fonts scale scroll-offsets images))

(define (ui-ctx-font ctx pt-size)
  ((ui-ctx-fonts ctx) pt-size))

(define current-hover-path (make-parameter '()))
(define current-focus-path (make-parameter '()))
;; Frame time in seconds — drives the spinner animation. The run loop sets
;; this every frame; snapshot rendering leaves 0 for deterministic output.
(define current-frame-time (make-parameter 0.0))

;; ---- laid tree ---------------------------------------------------------------------

(struct laid (node path x y w h children) #:transparent)

;; ---- measure: natural (preferred) size in points ----------------------------------

(define (measure-node ctx n)
  (define kids (filter node? (node-children n)))
  (match (node-kind n)
    ['text
     (define size (or (node-prop n 'size) (theme-font-size (theme-current))))
     (define fs (ui-ctx-font ctx size))
      (define f (and fs (car fs)))
     (values (if f (+ (text-width f (node-prop n 'content)) 1) (* 0.7 size))
             (* 1.3 size))]
    ['row
     (define spacing (node-prop n 'spacing 8))
     (define-values (w h)
       (for/fold ([acc-w 0.0] [acc-h 0.0])
                 ([k (in-list kids)]
                  [i (in-naturals)])
         (define-values (kw kh) (measure-node ctx k))
         (values (+ acc-w kw (if (zero? i) 0 spacing))
                 (max acc-h kh))))
     (values w h)]
    ['column
     (define spacing (node-prop n 'spacing 8))
     (define-values (w h)
       (for/fold ([acc-w 0.0] [acc-h 0.0])
                 ([k (in-list kids)]
                  [i (in-naturals)])
         (define-values (kw kh) (measure-node ctx k))
         (values (max acc-w kw)
                 (+ acc-h kh (if (zero? i) 0 spacing)))))
     (values w h)]
    ['spacer (values (node-prop n 'w 0) (node-prop n 'h 0))]
    ['box
     (define pad (node-prop n 'padding 16))
     (define fixed-w (node-prop n 'width))
     (define fixed-h (node-prop n 'height))
     (if (null? kids)
         (values (or fixed-w (* 2 pad)) (or fixed-h (* 2 pad)))
         (let-values ([(kw kh) (measure-node ctx (car kids))])
           (values (or fixed-w (+ kw (* 2 pad)))
                   (or fixed-h (+ kh (* 2 pad))))))]
    ['button
     (define size (theme-font-size (theme-current)))
     (define fs (ui-ctx-font ctx size))
     (define f (and fs (car fs)))
     (define label (node-prop n 'label))
     (define tw (if (and f (string? label)) (text-width f label)
                    (* 0.6 size (string-length (format "~a" label)))))
     (values (+ tw 28) (+ size 16))]
    ['checkbox
     (define size (theme-font-size (theme-current)))
     (define fs (ui-ctx-font ctx size))
     (define f (and fs (car fs)))
     (define label (node-prop n 'label))
     (define tw (if (and f (string? label)) (text-width f label) 0))
     (values (+ 16 tw 10) (+ size 6))]
    ['progress (values 120 (node-prop n 'height 6))]
    ['input (values 160 (+ (theme-font-size (theme-current)) 12))]
    ['divider (values 1 1)]
    ['scroll
     (define iw (node-prop n 'width))
     (define ih (node-prop n 'height))
     (values (or iw 200) (or ih 150))]
    ['slider (values 160 24)]
    ['image
     (define-values (iw ih)
       (if (hash-ref (ui-ctx-images ctx) (node-prop n 'src) #f)
           (let ([t (hash-ref (ui-ctx-images ctx) (node-prop n 'src))])
             (values (car t) (cadr t)))
           (image-size (node-prop n 'src))))
     (values (or (node-prop n 'width) iw)
             (or (node-prop n 'height) ih))]
    ['spinner
     (define sz (node-prop n 'size 20))
     (values sz sz)]
    [else (values 0 0)]))

;; ---- image cache ---------------------------------------------------------------------

;; src -> (list width height texture-id-or-#f); texture created lazily when
;; a GL context is current (see image-ensure-texture!).
(define image-cache (make-hash))

(define (image-size src)
  (hash-ref! image-cache src
             (λ ()
               (define-values (w h rgba) (image-load src))
               (list w h #f))))

(define (image-ensure-texture! src)
  (define entry (image-size src))
  (unless (vector-ref entry 2)
    (vector-set! entry 2 (renderer-texture-rgba (vector-ref entry 0) (vector-ref entry 1) #f)))
  (vector-ref entry 2))

;; ---- layout ------------------------------------------------------------------------


;; ---- layout ------------------------------------------------------------------------

;; Arrange one container's children; returns laid children (absolute coords).
(define (arrange-container ctx rec n kids inner-w inner-h path axis x y pad align spacing)
  (define naturals
    (for/list ([k (in-list kids)])
      (define-values (nw nh) (measure-node ctx k))
      (if (eq? axis 'row) nw nh)))
  (define flexes (for/list ([k (in-list kids)]) (node-flex k)))
  (define flex-total (apply + flexes))
  (define natural-total (apply + naturals))
  (define gaps (* spacing (max 0 (sub1 (length kids)))))
  (define cross-inner (if (eq? axis 'row) inner-h inner-w))
  (define extra (max 0 (- (if (eq? axis 'row) inner-w inner-h)
                          natural-total gaps)))
  (let loop ([kids kids] [nats naturals] [flxs flexes]
             [pos pad] [idx 0] [acc '()])
    (cond
      [(null? kids) (reverse acc)]
      [else
       (define k (car kids))
       (define nat (car nats))
       (define flx (car flxs))
       (define-values (cnw cnh) (measure-node ctx k))
       (define nat-cross (if (eq? axis 'row) cnh cnw))
       (define main-size
         (if (and (> flex-total 0) (> flx 0))
             (+ nat (* extra (/ flx flex-total)))
             nat))
       (define cross-size (if (eq? align 'stretch) cross-inner nat-cross))
       (define cross-off
         (match align
           ['center (/ (- cross-inner cross-size) 2)]
           ['end (- cross-inner cross-size)]
           [_ 0]))
       (define kx (+ x (if (eq? axis 'row) pos cross-off)))
       (define ky (+ y (if (eq? axis 'row) cross-off pos)))
       (define kw (if (eq? axis 'row) main-size cross-size))
       (define kh (if (eq? axis 'row) cross-size main-size))
       (loop (cdr kids) (cdr nats) (cdr flxs)
             (+ pos main-size spacing) (add1 idx)
             (cons (rec k (append path (list idx)) kx ky kw kh) acc))])))

(define (layout-view ctx node avail-w avail-h)
  (let rec ([n node] [path '()] [x 0.0] [y 0.0] [w avail-w] [h avail-h])
    (define kind (node-kind n))
    (define kids (filter node? (node-children n)))
    (define laid-kids
      (cond
        [(memq kind '(row column))
         (define pad (node-prop n 'padding 0))
         (define spacing (node-prop n 'spacing 8))
         (define align (node-prop n 'align (if (eq? kind 'row) 'start 'stretch)))
         (define inner-w (max 0 (- w (* 2 pad))))
         (define inner-h (max 0 (- h (* 2 pad))))
         (arrange-container ctx rec n kids inner-w inner-h path
                            (if (eq? kind 'row) 'row 'column)
                            (+ x pad) (+ y pad) pad align spacing)]
        [(eq? kind 'box)
         (define pad (node-prop n 'padding 16))
         (for/list ([k (in-list kids)] [idx (in-naturals)])
           (rec k (append path (list idx)) (+ x pad) (+ y pad)
                (max 0 (- w (* 2 pad))) (max 0 (- h (* 2 pad)))))]
        [else '()]))
    (laid n path x y w h laid-kids)))

(define (draw-laid! r laid ctx)
  (let rec ([l laid])
    (define n (laid-node l))
    (define x (laid-x l)) (define y (laid-y l))
    (define w (laid-w l)) (define h (laid-h l))
    (define path (laid-path l))
    (match (node-kind n)
      ['box
       (define pad (node-prop n 'padding 16))
       (define bg (node-prop n 'bg))
       (define radius (or (node-prop n 'radius) (theme-radius (theme-current))))
       (when bg (r-round! r x y w h radius bg))
       (define bc (node-prop n 'border-color))
       (when (and bc (node-prop n 'border))
         (r-ring! r x y w h radius (node-prop n 'border) bc))
       (for ([k (in-list (laid-children l))]) (rec k))]
      [(or 'row 'column)
       (for ([k (in-list (laid-children l))]) (rec k))]
      ['text
       (define size (or (node-prop n 'size) (theme-font-size (theme-current))))
       (define col (or (node-prop n 'color) (theme-text (theme-current))))
       (define fs (ui-ctx-font ctx size))
       (when fs
         (define f (car fs))
         (define content (node-prop n 'content))
         (define lh (* 1.3 size))
         (define y-off (/ (- lh (* 0.8 size)) 2))
         (draw-text! r fs content x (+ y y-off) col))]
      ['button (draw-button! r n ctx x y w h path)]
      ['checkbox (draw-checkbox! r n ctx x y w h path)]
      ['progress
       (define v (node-prop n 'value))
       (define col (or (node-prop n 'color) (theme-accent (theme-current))))
       (define bgc (or (node-prop n 'bg) (theme-border (theme-current))))
       (r-round! r x y w h 3 bgc)
       (when (> v 0)
         (r-round! r x y (max h (* w v)) h 3 col))]
      ['input (draw-input! r n ctx x y w h path)]
      ['divider
       (r-rect! r x y w 1 (or (node-prop n 'color) (theme-border (theme-current))))]
      ['scroll
       (r-scissor-push! r x y w h)
       (for ([k (in-list (laid-children l))]) (rec k))
       (r-scissor-pop! r)
       ;; scrollbar thumb: only when content overflows
       (define kid (and (not (null? (laid-children l))) (car (laid-children l))))
       (when kid
         (define max-off (max 1 (- (laid-h kid) h)))
         (define off (min max-off (max 0 (hash-ref (ui-ctx-scroll-offsets ctx) path 0))))
         (define thumb-h (max 24 (* h (/ h (laid-h kid)))))
         (define track-h (- h thumb-h))
         (define thumb-y (+ y (* track-h (/ off max-off))))
         (r-round! r (+ x w -4) (+ thumb-y 2) 4 (- thumb-h 4) 2 (theme-border (theme-current))))]
      ['slider
       (define v (node-prop n 'value))
       (define enabled? (node-prop n 'enabled? #t))
       (define t (theme-current))
       (define hover? (equal? path (current-hover-path)))
       (define track-y (+ y (/ h 2)))
       (define col (if enabled? (theme-accent t) (theme-text-faint t)))
       (r-round! r (+ x 9) (- track-y 3) (- w 18) 6 3 (theme-border t))
       (r-round! r (+ x 9) (- track-y 3) (max 6 (* (- w 18) v)) 6 3 col)
       (define kx (+ x 9 (* (- w 18) v)))
       (r-circle! r kx track-y 9 (if enabled? (theme-text t) (theme-text-faint t)))
       (r-circle! r kx track-y 7 (if (equal? path (current-focus-path)) (theme-surface-raised t) (theme-surface t)))]
      ['image
       (define src (node-prop n 'src))
       (define tex (image-ensure-texture! src))
       (r-use-texture! r tex)
       (r-quad-uv! r x y w h 0.0 0.0 1.0 1.0 (color 1 1 1 1))]
      ['spinner
       (define sz (node-prop n 'size 20))
       (define col (or (node-prop n 'color) (theme-accent (theme-current))))
       (define cx (+ x (/ w 2)))
       (define cy (+ y (/ h 2)))
       (define rot (* 2.4 (current-frame-time)))
       (for ([i (in-range 8)])
         (define a (+ rot (* i (/ (* 2 pi) 8))))
         (define dx (* (/ sz 2) (cos a)))
         (define dy (* (/ sz 2) (sin a)))
         (define alpha (+ 0.15 (* 0.85 (/ i 8))))
         (r-circle! r (+ cx dx) (+ cy dy) (- (/ sz 8) 1)
                    (color (color-r col) (color-g col) (color-b col) alpha)))]
      ['divider
       (r-rect! r x y w 1 (or (node-prop n 'color) (theme-border (theme-current))))]
      ['spacer (void)]
      [_ (void)])))

(define (draw-button! r n ctx x y w h path)
  (define t (theme-current))
  (define kind (node-prop n 'kind 'primary))
  (define enabled? (node-prop n 'enabled? #t))
  (define hover? (equal? path (current-hover-path)))
  (define base
    (match kind
      ['primary (theme-accent t)]
      ['danger (theme-danger t)]
      ['secondary (theme-surface t)]
      ['ghost #f]
      [_ (theme-accent t)]))
  (define fill
    (cond
      [(not enabled?) (color-scale (or base (theme-surface t)) 0.55)]
      [(and hover? base) (color-scale base 0.9)]
      [hover? (theme-surface-raised t)]
      [else base]))
  (when fill (r-round! r x y w h (theme-radius t) fill))
  (when (eq? kind 'secondary)
    (r-ring! r x y w h (theme-radius t) 1 (theme-border t)))
  (define label (node-prop n 'label))
  (define size (theme-font-size t))
  (define fs (ui-ctx-font ctx size))
  (define f (and fs (car fs)))
  (when f
    (define txt-col
      (cond
        [(not enabled?) (theme-text-faint t)]
        [(memq kind '(secondary ghost)) (theme-text t)]
        [else (theme-on-accent t)]))
    (define tw (text-width f label))
    (draw-text! r fs label (+ x (/ (- w tw) 2)) (+ y (/ (- h size) 2)) txt-col)))

(define (draw-checkbox! r n ctx x y w h path)
  (define t (theme-current))
  (define size (theme-font-size t))
  (define checked? (node-prop n 'checked?))
  (define enabled? (node-prop n 'enabled? #t))
  (define box-s (- size 2))
  (define box-y (+ y (/ (- h box-s) 2)))
  (cond
    [checked?
     (r-round! r x box-y box-s box-s 4 (theme-accent t))
     (define cx1 (+ x (* box-s 0.22)))
     (define cy1 (+ box-y (* box-s 0.55)))
     (define cx2 (+ x (* box-s 0.42)))
     (define cy2 (+ box-y (* box-s 0.75)))
     (define cx3 (+ x (* box-s 0.78)))
     (define cy3 (+ box-y (* box-s 0.28)))
     (define on-accent (theme-on-accent t))
     (r-line! r cx1 cy1 cx2 cy2 2 on-accent)
     (r-line! r cx2 cy2 cx3 cy3 2 on-accent)]
    [else
     (r-round! r x box-y box-s box-s 4 (theme-input-bg t))
     (r-ring! r x box-y box-s box-s 4 1 (theme-border t))])
  (define fs (ui-ctx-font ctx size))
  (define f (and fs (car fs)))
  (when f
    (define col (if enabled? (theme-text t) (theme-text-faint t)))
    (draw-text! r fs (node-prop n 'label) (+ x box-s 10) (+ y (/ (- h size) 2)) col)))

(define (draw-input! r n ctx x y w h path)
  (define t (theme-current))
  (define size (theme-font-size t))
  (define focused? (equal? path (current-focus-path)))
  (r-round! r x y w h 6 (theme-input-bg t))
  (r-ring! r x y w h 6 (if focused? 2 1)
           (if focused? (theme-accent t) (theme-border t)))
  (define fs (ui-ctx-font ctx size))
  (define f (and fs (car fs)))
  (when f
    (define value (node-prop n 'value))
    (define shown
      (if (node-prop n 'password?)
          (make-string (string-length value) #\u2022)
          value))
    (cond
      [(and (string=? value "") (not (string=? (node-prop n 'placeholder "") "")))
       (draw-text! r fs (node-prop n 'placeholder) (+ x 8) (+ y (/ (- h size) 2))
                   (theme-text-faint t))]
      [else
       (draw-text! r fs shown (+ x 8) (+ y (/ (- h size) 2)) (theme-text t))
       (when focused?
         (define cw (text-width f shown))
         (r-rect! r (+ x 8 cw 2) (+ y (/ (- h size) 2)) 1 size
                  (theme-text t)))])))

;; ---- hit testing ------------------------------------------------------------------------

;; Deepest laid node containing (px, py); later siblings win (topmost).
(define (hit-test l px py)
  (let rec ([l l])
    (or (for/first ([k (in-list (reverse (laid-children l)))]
                    #:when (in-rect? k px py))
          (rec k))
        (and (in-rect? l px py) l))))

(define (in-rect? l px py)
  (and (>= px (laid-x l)) (>= py (laid-y l))
       (<= px (+ (laid-x l) (laid-w l)))
       (<= py (+ (laid-y l) (laid-h l)))))

;; Deepest interactive laid node (button/checkbox/input) containing the
;; point. Interactive widgets are leaves, so the deepest hit inside the
;; rect is the widget itself.
(define (hit-interactive l px py)
  (let rec ([l l])
    (or (for/first ([k (in-list (reverse (laid-children l)))]
                    #:when (in-rect? k px py))
          (rec k))
        (and (memq (node-kind (laid-node l)) '(button checkbox input))
             (in-rect? l px py)
             l))))

;; Find the laid node with the given path.
(define (path->laid l path)
  (cond
    [(equal? (laid-path l) path) l]
    [else
     (for/first ([k (in-list (laid-children l))])
       (define hit (path->laid k path))
       (and hit hit))]))
