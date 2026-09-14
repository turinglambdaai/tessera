#lang racket/base

;; tessera's renderer.
;;
;; Fixed-function OpenGL (GL 1.3-era surface, GL 2.1 context), chosen over a
;; shader pipeline deliberately: every driver ships it — including macOS 26,
;; whose shader-compiler paths fail inside non-bundled processes (fragment
;; shader creation is rejected outright) — and a UI's needs are modest:
;; batched quads, gradients via per-vertex color, tinted glyphs, MSAA
;; smoothing for CPU-tessellated rounded corners.
;;
;; Vertex stream (interleaved, pushed via client arrays, drawn with
;; glDrawArrays):
;;
;;   x y r g b a u v    — 8 floats / 32 bytes per vertex
;;
;; Anti-aliasing comes from 4x MSAA requested by the platform layer;
;; rounded rectangles are tessellated into polygon fans, borders into
;; rings, so the fragment stage stays fixed-function.
;;
;; All draw calls take POINTS; the renderer converts with the frame scale
;; and snaps to whole device pixels.

(require ffi/unsafe
         racket/list
         racket/match
         racket/math
         racket/string
         racket/format
         tessera/ffi/gl)

(provide (struct-out renderer)
         (struct-out color)
         color-hex
         color-argb
         color-scale
         make-renderer
         renderer-begin-frame!
         renderer-end-frame!
         renderer-clear!
         r-rect!
         r-quad-grad!
         r-round!
         r-ring!
         r-line!
         r-circle!
         r-quad-uv!
         r-use-texture!
         r-scissor-push!
         r-scissor-pop!
         renderer-texture-alpha
         renderer-texture-rgba
         renderer-delete-texture
         renderer-read-pixels)

;; ---- colors -------------------------------------------------------------------

(struct color (r g b a) #:transparent)   ; components 0..1

(define (byte->f b) (/ b 255.0))

(define (color-hex s [a 1.0])
  (define str (string-trim (if (string? s) s (format "~a" s)) "#"))
  (unless (= 6 (string-length str))
    (error 'color-hex "expected #RRGGBB, got ~a" s))
  (define n (string->number str 16))
  (color (byte->f (bitwise-and #xFF (arithmetic-shift n -16)))
         (byte->f (bitwise-and #xFF (arithmetic-shift n -8)))
         (byte->f (bitwise-and #xFF n))
         a))

(define (color-argb argb)
  (color (byte->f (bitwise-and #xFF (arithmetic-shift argb -16)))
         (byte->f (bitwise-and #xFF (arithmetic-shift argb -8)))
         (byte->f (bitwise-and #xFF argb))
         (byte->f (bitwise-and #xFF (arithmetic-shift argb -24)))))

(define (color-scale c k)
  (color (* (color-r c) k) (* (color-g c) k) (* (color-b c) k) (color-a c)))

;; ---- renderer state --------------------------------------------------------------

(define max-verts 131072)
(define floats-per-vert 8)

(struct renderer (vbuf                     ; malloc'd cpointer, scratch vertices
                  vcount                   ; floats consumed in the current batch
                  tex                      ; texture bound to the current batch (#f = none)
                  scissor                  ; current scissor in points: (vec x y w h) or #f
                  fbw fbh scale)
  #:mutable #:transparent)

(define (make-texture-raw internal fmt w h bytes)
  (define tex (malloc _uint 1 'raw))
  (ptr-set! tex _uint 0 0)
  (glGenTextures 1 tex)
  (define id (ptr-ref tex _uint))
  (glBindTexture GL_TEXTURE_2D id)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  (glTexImage2D GL_TEXTURE_2D 0 internal w h 0 fmt GL_UNSIGNED_BYTE
                (if bytes bytes #f))
  id)

;; Alpha (coverage) texture for font atlases: fixed-function MODULATE keeps
;; rgb from glColor and takes alpha from the texture.
(define (renderer-texture-alpha w h bytes)
  (make-texture-raw GL_ALPHA GL_ALPHA w h bytes))

;; RGBA texture for images.
(define (renderer-texture-rgba w h bytes)
  (make-texture-raw GL_RGBA GL_RGBA w h bytes))

(define (renderer-delete-texture id)
  (define arr (malloc _uint 1 'raw))
  (ptr-set! arr _uint 0 id)
  (glDeleteTextures 1 arr))

(define (make-renderer)
  (define vbuf (malloc _byte (* max-verts floats-per-vert 4) 'raw))
  (glEnableClientState GL_VERTEX_ARRAY)
  (glVertexPointer 2 GL_FLOAT 32 vbuf)
  (glEnableClientState GL_COLOR_ARRAY)
  (glColorPointer 4 GL_FLOAT 32 (ptr-add vbuf 8))
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
  (glTexCoordPointer 2 GL_FLOAT 32 (ptr-add vbuf 24))
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (renderer vbuf 0 #f #f 0 0 1.0))

;; ---- coordinate helpers -------------------------------------------------------------

(define (px r v) (exact-round (* v (renderer-scale r))))

;; ---- batching ------------------------------------------------------------------------

(define (use-text-state! r tex)
  (unless (equal? tex (renderer-tex r))
    (flush! r)
    (set-renderer-tex! r tex)
    (cond
      [tex
       (glEnable GL_TEXTURE_2D)
       (glBindTexture GL_TEXTURE_2D tex)]
      [else
       (glDisable GL_TEXTURE_2D)])))

(define (flush! r)
  (define n (renderer-vcount r))
  (when (> n 0)
    (glDrawArrays GL_TRIANGLES 0 (quotient n 3))
    (set-renderer-vcount! r 0)))

(define (ensure-capacity! r extra-verts)
  (when (> (+ (quotient (renderer-vcount r) floats-per-vert) extra-verts) max-verts)
    (flush! r)))

(define (set-scissor! r clip)
  (flush! r)
  (set-renderer-scissor! r clip)
  (apply-scissor! r clip))

(define (apply-scissor! r clip)
  (if clip
      (match-let ([(vector x y w h) clip])
        (glEnable GL_SCISSOR_TEST)
        (glScissor (px r x)
                   (- (renderer-fbh r) (px r (+ y h)))
                   (px r w)
                   (px r h)))
      (begin
        (glDisable GL_SCISSOR_TEST)
        (glScissor 0 0 (renderer-fbw r) (renderer-fbh r)))))

(define (r-scissor-push! r x y w h)
  (define parent (renderer-scissor r))
  (define next
    (if parent
        (match-let ([(vector px0 py0 pw0 ph0) parent])
          (vector (max x px0)
                  (max y py0)
                  (min (+ x w) (+ px0 pw0))
                  (min (+ y h) (+ py0 ph0))))
        (vector x y w h)))
  (match-let ([(vector nx ny nw nh) next])
    (set-scissor! r next)))

(define (r-scissor-pop! r)
  (flush! r)
  (set-renderer-scissor! r #f)
  (apply-scissor! r #f))

(define (r-use-texture! r tex)
  (use-text-state! r tex))

;; Push one triangle (three vertices). u/v land in the texture coord slot.
(define (push-tri! r x0 y0 x1 y1 x2 y2 c u v)
  (ensure-capacity! r 3)
  (define buf (renderer-vbuf r))
  (define base (renderer-vcount r))
  (define (put off val) (ptr-set! buf _float (+ base off) (exact->inexact val)))
  (put 0 x0) (put 1 y0)
  (put 2 (color-r c)) (put 3 (color-g c)) (put 4 (color-b c)) (put 5 (color-a c))
  (put 6 u) (put 7 v)
  (put 8 x1) (put 9 y1)
  (put 10 (color-r c)) (put 11 (color-g c)) (put 12 (color-b c)) (put 13 (color-a c))
  (put 14 u) (put 15 v)
  (put 16 x2) (put 17 y2)
  (put 18 (color-r c)) (put 19 (color-g c)) (put 20 (color-b c)) (put 21 (color-a c))
  (put 22 u) (put 23 v)
  (set-renderer-vcount! r (+ base (* 3 floats-per-vert))))

;; Quad as two triangles, one color.
(define (push-quad! r x0 y0 x1 y1 c)
  (push-tri! r x0 y0 x1 y0 x0 y1 c 0.0 0.0)
  (push-tri! r x1 y0 x1 y1 x0 y1 c 0.0 0.0))

;; Quad with four explicit corner colors (gradients) and a uv rect.
(define (push-quad-corners! r x0 y0 x1 y1 c0 c1 c2 c3 u0 v0 u1 v1)
  (ensure-capacity! r 6)
  (define buf (renderer-vbuf r))
  (define base (renderer-vcount r))
  (define (vert off px-x px-y col u v)
    (ptr-set! buf _float (+ base off) (exact->inexact px-x))
    (ptr-set! buf _float (+ base off 1) (exact->inexact px-y))
    (ptr-set! buf _float (+ base off 2) (exact->inexact (color-r col)))
    (ptr-set! buf _float (+ base off 3) (exact->inexact (color-g col)))
    (ptr-set! buf _float (+ base off 4) (exact->inexact (color-b col)))
    (ptr-set! buf _float (+ base off 5) (exact->inexact (color-a col)))
    (ptr-set! buf _float (+ base off 6) (exact->inexact u))
    (ptr-set! buf _float (+ base off 7) (exact->inexact v)))
  ;; triangle 1: TL TR BL
  (vert 0  x0 y0 c0 u0 v0)
  (vert 8  x1 y0 c1 u1 v0)
  (vert 16 x0 y1 c2 u0 v1)
  ;; triangle 2: TR BR BL
  (vert 24 x1 y0 c1 u1 v0)
  (vert 32 x1 y1 c3 u1 v1)
  (vert 40 x0 y1 c2 u0 v1)
  (set-renderer-vcount! r (+ base (* 6 floats-per-vert))))

;; ---- draw calls (points in, device px internally) --------------------------------------

;; Plain filled rectangle (optionally a per-corner gradient).
(define (r-rect! r x y w h c [c2 c] [c3 c] [c4 c])
  (use-text-state! r #f)
  (push-quad-corners! r (px r x) (px r y) (px r (+ x w)) (px r (+ y h))
                      c c2 c3 c4 0.0 0.0 0.0 0.0))

(define (r-quad-grad! r x y w h top bottom)
  (r-rect! r x y w h top top bottom bottom))

;; Arc segments per quarter circle for tessellation.
(define arc-segments 6)

(define (rounded-outline px-x px-y px-w px-h radius)
  ;; outline points (device px) of a rounded rect, clockwise from the
  ;; top-left corner arc; returns a list of (x . y)
  (define r (min radius (* 0.5 (min px-w px-h))))
  (define x0 px-x) (define y0 px-y)
  (define x1 (+ px-x px-w)) (define y1 (+ px-y px-h))
  (define pts '())
  (define (arc cx cy a0 a1)
    (for ([i (in-range (add1 arc-segments))])
      (define a (+ a0 (* (- a1 a0) (/ i arc-segments))))
      (set! pts (cons (cons (+ cx (* r (cos a))) (+ cy (* r (sin a)))) pts))))
  ;; clockwise in top-down screen space
  (arc (+ x0 r) (+ y0 r) pi (* 1.5 pi))
  (arc (- x1 r) (+ y0 r) (* 1.5 pi) (* 2.0 pi))
  (arc (- x1 r) (- y1 r) 0.0 (* 0.5 pi))
  (arc (+ x0 r) (- y1 r) (* 0.5 pi) pi)
  (reverse pts))

;; Rounded-rect fill: triangle fan from the bounding-box center.
;; The outline is treated as cyclic: vertex k pairs with vertex k+1,
;; and the last wraps back to the first, closing the fan.
(define (r-round! r x y w h radius c)
  (use-text-state! r #f)
  (define x0 (px r x)) (define y0 (px r y))
  (define x1 (px r (+ x w))) (define y1 (px r (+ y h)))
  (define pts (rounded-outline x0 y0 (- x1 x0) (- y1 y0) (px r radius)))
  (define cx (* 0.5 (+ x0 x1)))
  (define cy (* 0.5 (+ y0 y1)))
  (define n (length pts))
  (let loop ([k 0])
    (when (< k n)
      (let ([a (list-ref pts k)]
            [b (list-ref pts (modulo (add1 k) n))])
        (push-tri! r cx cy (car a) (cdr a) (car b) (cdr b) c 0.0 0.0))
      (loop (add1 k)))))

;; Circle via fan.
(define (r-circle! r cx cy radius c)
  (r-round! r (- cx radius) (- cy radius) (* 2 radius) (* 2 radius) radius c))

;; Rounded-rect border: strip between the outer outline and an inset copy.
(define (r-ring! r x y w h radius border c)
  (use-text-state! r #f)
  (define x0 (px r x)) (define y0 (px r y))
  (define x1 (px r (+ x w))) (define y1 (px r (+ y h)))
  (define bw (px r border))
  (define outer (rounded-outline x0 y0 (- x1 x0) (- y1 y0) (px r radius)))
  (define inner (rounded-outline (+ x0 bw) (+ y0 bw)
                                 (- (- x1 x0) (* 2 bw)) (- (- y1 y0) (* 2 bw))
                                 (max 0 (- (px r radius) bw))))
  (define n (min (length outer) (length inner)))
  (define o (list->vector (take outer n)))
  (define i (list->vector (take inner n)))
  (let loop ([k 0])
    (when (< k n)
      (define a (vector-ref o k))
      (define b (vector-ref o (if (= k (sub1 n)) 0 (+ k 1))))
      (define p (vector-ref i k))
      (define q (vector-ref i (if (= k (sub1 n)) 0 (+ k 1))))
      (push-tri! r (car a) (cdr a) (car b) (cdr b) (car p) (cdr p) c 0.0 0.0)
      (push-tri! r (car b) (cdr b) (car q) (cdr q) (car p) (cdr p) c 0.0 0.0)
      (loop (add1 k)))))

;; Thick line from (x0,y0) to (x1,y1) as a rotated quad with round caps.
(define (r-line! r x0 y0 x1 y1 width c)
  (use-text-state! r #f)
  (define dx (- x1 x0)) (define dy (- y1 y0))
  (define len (sqrt (+ (* dx dx) (* dy dy))))
  (when (> len 0.0001)
    (define ux (/ dx len)) (define uy (/ dy len))
    (define hw (/ width 2.0))
    (define nx (* -1.0 uy hw)) (define ny (* ux hw))
    (define ax (px r (+ x0 nx))) (define ay (px r (+ y0 ny)))
    (define bx (px r (+ x1 nx))) (define by (px r (+ y1 ny)))
    (define cx2 (px r (+ x1 (- nx)))) (define cy2 (px r (+ y1 (- ny))))
    (define dxp (px r (+ x0 (- nx)))) (define dyp (px r (+ y0 (- ny))))
    (push-tri! r ax ay bx by dxp dyp c 0.0 0.0)
    (push-tri! r bx by cx2 cy2 dxp dyp c 0.0 0.0)
    (r-circle! r x0 y0 hw c)
    (r-circle! r x1 y1 hw c)))

;; Textured quad (text glyph or image), uv in normalized texture coords.
;; The caller binds the texture with r-use-texture! first.
(define (r-quad-uv! r x y w h u0 v0 u1 v1 c)
  (push-quad-corners! r (px r x) (px r y) (px r (+ x w)) (px r (+ y h))
                      c c c c u0 v0 u1 v1))

;; ---- frame lifecycle ---------------------------------------------------------------------

(define (renderer-begin-frame! r fbw fbh scale)
  (set-renderer-fbw! r fbw)
  (set-renderer-fbh! r fbh)
  (set-renderer-scale! r scale)
  (set-renderer-vcount! r 0)
  (set-renderer-tex! r 'unset)
  (set-renderer-scissor! r #f)
  (glViewport 0 0 fbw fbh)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;; top-down UI coordinates: +y grows downward
  (glOrtho 0.0 (exact->inexact fbw) (exact->inexact fbh) 0.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glDisable GL_SCISSOR_TEST)
  (glEnable GL_MULTISAMPLE)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))

(define (renderer-clear! r c)
  (glClearColor (color-r c) (color-g c) (color-b c) (color-a c))
  (glClear GL_COLOR_BUFFER_BIT))

(define (renderer-end-frame! r)
  (flush! r))

;; Read back the framebuffer as top-left-origin RGBA bytes (for snapshots).
(define (renderer-read-pixels r)
  (define w (renderer-fbw r))
  (define h (renderer-fbh r))
  (define raw (make-bytes (* w h 4)))
  (glReadPixels 0 0 w h GL_RGBA GL_UNSIGNED_BYTE raw)
  ;; flip vertically (GL origin is bottom-left)
  (define stride (* w 4))
  (define out (make-bytes (bytes-length raw)))
  (for ([y (in-range h)])
    (bytes-copy! out (* y stride) raw (* (- h 1 y) stride) (* (- h y) stride)))
  out)
