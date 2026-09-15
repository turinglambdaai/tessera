#lang racket/base

;; The view layer: declarative, immutable widget descriptions.
;;
;; A view is a tree of `node` values — plain data. `layout-view` measures
;; and arranges the tree; `draw-laid!` lowers it to renderer calls; event
;; handlers on nodes produce messages for the app's update loop.
;;
;; Constructors accept keywords in any position, so both of these work:
;;
;;   (column #:spacing 12 (button "ok") (text "hi"))
;;   (column (button "ok") (text "hi") #:spacing 12)
;;
;; Children may include #f (filtered out) for conditional UI.

(require racket/format
         racket/hash
         racket/list
         racket/match
         racket/string)

(provide (struct-out node)
         node-prop
         node-flex
         text
         row
         column
         spacer
         box
         button
         checkbox
         input
         progress
         divider
         scroll
         slider
         image
         spinner
         modal
         view?)

;; ---- node model ------------------------------------------------------------------

(struct node (kind props children) #:transparent)

(define (node-prop n key [default #f])
  (hash-ref (node-props n) key default))

;; Flex weight: share of leftover main-axis space this node absorbs.
(define (node-flex n)
  (hash-ref (node-props n) 'flex 0))

(define (view? v) (node? v))

;; ---- keyword plumbing ---------------------------------------------------------------

;; Build the props hash from keyword arguments; keywords become prop names
;; without the #: marker. #f values are dropped so defaults apply.
(define (kws->props kws kw-vals)
  (for/hash ([k (in-list kws)] [v (in-list kw-vals)] #:when v)
    (values (string->symbol (keyword->string k)) v)))

(define (node-builder kind [main-prop #f])
  ;; main-prop: leaf widgets (text/button/...) take a primary value as the
  ;; first positional argument; containers treat every positional as a child.
  (make-keyword-procedure
   (λ (kws kw-vals . args)
      (define props (kws->props kws kw-vals))
      (define-values (children extra)
        (cond
          [(not main-prop) (values args (hasheq))]
          [(null? args) (values '() (hasheq))]
          [else (values (cdr args) (hash main-prop (car args)))]))
      (node kind (hash-union props extra) (filter node? children)))))

(define text (node-builder 'text 'content))
(define row (node-builder 'row))
(define column (node-builder 'column))
(define spacer (node-builder 'spacer))
(define box (node-builder 'box))
(define button (node-builder 'button 'label))
(define checkbox (node-builder 'checkbox 'label))
(define input (node-builder 'input 'value))
(define progress (node-builder 'progress 'value))
(define divider (node-builder 'divider))

;; Scrollable viewport: content taller than the box scrolls with the wheel.
;; Place inside a bounded container (a column's remaining space, or give an
;; explicit #:height / #:width). Children are clipped to the viewport.
(define scroll
  (make-keyword-procedure
   (λ (kws kw-vals . children)
      (node 'scroll (kws->props kws kw-vals) (filter node? children)))))

;; Drag control. value is in [min, max]; on-change receives the new value
;; continuously while dragging.
(define slider
  (node-builder 'slider 'value))

;; Raster image from a .qoi/.bmp/.tga file. Natural size = bitmap size,
;; optionally fitted into #:width x #:height (contain).
(define image
  (node-builder 'image 'src))

;; Indeterminate activity indicator (animated; needs a running `run` loop).
(define spinner
  (node-builder 'spinner))

;; Modal overlay: dims the whole window and floats its children centered.
;; While present, clicks outside the content are swallowed and Tab cycling
;; is restricted to the modal subtree. Place it anywhere in the tree.
(define modal
  (make-keyword-procedure
   (λ (kws kw-vals . children)
      (node 'modal (kws->props kws kw-vals) (filter node? children)))))

;; ---- constructor reference (props consumed by tessera/layout) -----------------------
;;
;; (text content #:size #:color #:align)            align: left|center|right
;; (row kids... #:spacing #:padding #:align)        align: start|center|end|stretch
;; (column kids... #:spacing #:padding #:align)
;; (spacer #:w #:h #:flex)
;; (box kids... #:padding #:bg #:radius #:border #:border-color #:width #:flex)
;; (button label #:on-click #:kind #:enabled? #:flex)   kind: primary|secondary|ghost|danger
;; (checkbox label #:checked? #:on-change #:enabled?)
;; (input value #:on-change #:placeholder #:password? #:enabled? #:flex)
;; (progress value #:height #:color #:bg)
;; (divider #:color)
;;
;; node props (read by tessera/layout):
;;   text: content size color align
;;   row/column: spacing padding align
;;   box: padding bg radius border border-color width
;;   button: label on-click kind enabled?
;;   checkbox: label checked? on-change enabled?
;;   input: value on-change placeholder password? enabled?
;;   progress: value height color bg
