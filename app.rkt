#lang racket/base

;; The application runtime: an Elm-style loop.
;;
;;   (run #:title "App"
;;        #:init-state 0
;;        #:update (lambda (state msg) new-state)
;;        #:view   (lambda (state) view-tree))
;;
;; Each frame: rebuild the view -> layout -> draw -> swap; then poll
;; events -> route them to widget callbacks -> fold the returned messages
;; into state. Interaction bookkeeping (hover, focus, caret, scroll
;; offsets, slider drags) lives here, keyed by stable tree paths.

(require racket/async-channel
         racket/contract
         racket/list
         racket/match
         racket/math
         racket/string
         tessera/platform
         tessera/ffi/glfw
         tessera/ffi/gl
         tessera/render
         tessera/text
         tessera/theme
         tessera/view
         tessera/layout)

(provide run
         post!
         quit!)

(define msg-channel (make-async-channel #f))

;; Post a message to the running app from any thread.
(define (post! msg) (async-channel-put msg-channel msg))

;; Ask the running app to close (cooperative; next loop tick exits).
(define (quit!) (async-channel-put msg-channel '(tessera:quit)))

(define (non-empty-string? s) (and (string? s) (> (string-length s) 0)))

(define (run #:title [title "tessera"]
             #:width [width 960]
             #:height [height 640]
             #:min-width [min-width #f]
             #:min-height [min-height #f]
             #:resizable? [resizable? #t]
             #:theme [theme theme:light]
             #:init-state [init-state (void)]
             #:update [update (lambda (s m) s)]
             #:view view
             #:on-frame [on-frame #f]
             #:font-path [font-path #f]
             #:quit-key [quit-key #\q]
             #:quit-mods [quit-mods 'super])
  (define pw
    (open-platform-window! #:width width #:height height
                           #:title title
                           #:resizable? resizable?
                           #:min-width min-width
                           #:min-height min-height
                           #:visible? #f))
  (define win (platform-window-glfw pw))

  (define renderer (make-renderer))
  (define font-file
    (or font-path
        (with-handlers ([exn:fail? (lambda (_e) #f)]) (find-font-file))))
  (unless font-file
    (error 'run "no usable TrueType font found; pass #:font-path or set TESSERA_FONT"))
  (define cjk-font-file
    (with-handlers ([exn:fail? (lambda (_e) #f)])
      (find-font-file #:require-glyph (integer->char #x4e2d))))

  ;; Per-frame font provider: primary face + CJK fallback face. Scroll
  ;; offsets and the image-texture cache persist across frames and resizes.
  (define scroll-offsets (make-hash))   ; path -> px offset
  (define image-cache (make-hash))      ; src -> (vector w h texture-id)
  (define (make-font-ctx scale)
    (make-ui-ctx
     (lambda (pt) (make-font-set font-file (exact-round (* pt scale))))
     (and cjk-font-file
          (lambda (pt) (make-font-set cjk-font-file (exact-round (* pt scale)))))
     scale
     scroll-offsets
     image-cache))

  ;; ---- mutable interaction state (main thread only) ---------------------------
  (define state init-state)
  (define laid-root #f)
  (define mx 0.0)
  (define my 0.0)
  (define focus-path '())
  (define focus-value "")
  (define caret 0)
  (define quit? #f)
  (define shown? #f)
  (define drag-slider-path #f)
  (define sel-anchor #f)        ; selection anchor caret, or #f
  (define last-ctx #f)          ; most recent font ctx (input hit tests)
  (define input-press? #f)      ; mouse is down on the focused input
  (define events '())

  (define (push-event! e) (set! events (cons e events)))

  ;; ---- message helpers ---------------------------------------------------------

  (define (norm-msgs v)
    (cond
      [(not v) '()]
      [(pair? v)
       (let flat ([x v] [acc '()])
         (cond
           [(pair? x) (flat (car x) (flat (cdr x) acc))]
           [(null? x) acc]
           [else (cons x acc)]))]
      [else (list v)]))

  (define (send! msgs)
    (for ([m (in-list (norm-msgs msgs))])
      (set! state (update state m))))

  ;; ---- GLFW callbacks (run on this thread during poll-events!) -----------------

  (glfwSetMouseButtonCallback
   win
   (lambda (w button action mods)
     (when (= button GLFW_MOUSE_BUTTON_1)
       (push-event! (list 'click button action)))))
  (glfwSetCursorPosCallback
   win
   (lambda (w x y)
     (set! mx x) (set! my y)
     ;; extend the input selection while drag-selecting
     (when (and input-press? laid-root (not (null? focus-path)))
       (define l (path->laid laid-root focus-path))
       (when (and l (eq? (node-kind (laid-node l)) 'input))
         (define fs (ui-ctx-font last-ctx (theme-font-size (theme-current))))
         (when fs
           (define fset (car fs))
           (define x-off (- x (+ (laid-x l) 8)))
           (set! caret (x->caret fset focus-value (max 0 x-off))))))))
  (glfwSetKeyCallback
   win
   (lambda (w key scancode action mods)
     (unless (zero? action)
       (push-event! (list 'key key action mods)))))
  (glfwSetCharCallback
   win
   (lambda (w cp) (push-event! (list 'char cp))))
  (glfwSetScrollCallback
   win
   (lambda (w xdy ydy) (push-event! (list 'wheel xdy ydy))))

  ;; ---- event handlers ------------------------------------------------------------

  ;; Deepest scroll container under the point (its laid node), or #f.
  (define (scroll-under l px py)
    (let rec ([l l])
      (or (for/first ([k (in-list (reverse (laid-children l)))]
                      #:when (in-rect? k px py))
            (rec k))
          (and (eq? (node-kind (laid-node l)) 'scroll)
               (in-rect? l px py)
               l))))

  ;; Adjust the deepest scroll container under the cursor by dy lines.
  (define (handle-wheel! ydy)
    (when laid-root
      (define target (scroll-under laid-root mx my))
      (when target
        (define path (laid-path target))
        (define content-h
          (if (null? (laid-children target))
              0
              (laid-h (car (laid-children target)))))
        (define max-off (max 0 (- content-h (laid-h target))))
        (define off (hash-ref scroll-offsets path 0))
        (hash-set! scroll-offsets path
                   (min max-off (max 0 (- off (* ydy 40))))))))

  (define (handle-click! action)
    (define hit (and laid-root (hit-interactive laid-root mx my)))
    (define pressed? (= action GLFW_PRESS))
    ;; slider drag lifecycle
    (set! drag-slider-path
          (if pressed?
              (and hit (eq? (node-kind (laid-node hit)) 'slider) (laid-path hit))
              #f))
    (when (and hit pressed?)
      (define n (laid-node hit))
      (match (node-kind n)
        ['slider
         (when (node-prop n 'enabled? #t)
           (slider-apply! hit mx))]
        ['button
         (when (node-prop n 'enabled? #t)
           (define cb (node-prop n 'on-click))
           (when cb (send! (cb))))]
        ['checkbox
         (when (node-prop n 'enabled? #t)
           (define cb (node-prop n 'on-change))
           (when cb (send! (cb (not (node-prop n 'checked?))))))]
        ['input
         (when (node-prop n 'enabled? #t)
           (set! focus-path (laid-path hit))
           (set! focus-value (node-prop n 'value))
           (define fs (ui-ctx-font last-ctx (theme-font-size (theme-current))))
           (set! caret
                 (if fs
                     (x->caret (car fs) focus-value
                               (max 0 (- mx (+ (laid-x hit) 8))))
                     (string-length focus-value)))
           (set! sel-anchor caret)
           (set! input-press? #t))]
        [_ (void)])))

  (define (slider-apply! l cursor-x)
    (define n (laid-node l))
    (define cb (node-prop n 'on-change))
    (when cb
      (define x (laid-x l))
      (define w (laid-w l))
      (define frac (min 1.0 (max 0.0 (/ (- cursor-x (+ x 9)) (max 1 (- w 18))))))
      (define lo (node-prop n 'min 0))
      (define hi (node-prop n 'max 1))
      (send! (cb (+ lo (* frac (- hi lo)))))))

  ;; While a slider drag is active, recompute its value from the cursor.
  (define (update-slider-drag!)
    (when (and drag-slider-path laid-root)
      (define l (path->laid laid-root drag-slider-path))
      (when (and l (<= mx (+ (laid-x l) (laid-w l))) (>= mx (laid-x l)))
        (slider-apply! l mx))))

  (define (backspace!)
    (when (> caret 0)
      (set! focus-value
            (string-append (substring focus-value 0 (sub1 caret))
                           (substring focus-value caret)))
      (set! caret (sub1 caret))
      (dispatch-input-change!)))

  ;; selection is (cons lo hi) caret indices, or #f when collapsed
  (define (sel-range)
    (and sel-anchor (not (= sel-anchor caret))
         (cons (min sel-anchor caret) (max sel-anchor caret))))
  (define (sel-delete!)
    (define r (sel-range))
    (when r
      (set! caret (car r))
      (set! sel-anchor caret)
      (set! focus-value
            (string-append (substring focus-value 0 (car r))
                           (substring focus-value (cdr r))))))

  (define (handle-key! key action mods)
    (define ctrl? (not (zero? (bitwise-and mods GLFW_MOD_CONTROL))))
    (define super? (not (zero? (bitwise-and mods GLFW_MOD_SUPER))))
    (define alt? (not (zero? (bitwise-and mods GLFW_MOD_ALT))))
    (when (= action GLFW_PRESS)
      ;; quit chord
      (when (and (= key (char->integer (char-upcase quit-key)))
                 (match quit-mods
                   ['super (or super? ctrl?)]
                   ['control ctrl?]
                   ['alt alt?]
                   [#f #f]))
        (set! quit? #t))
      (cond
        ;; text editing on the focused input
        [(and (not (null? focus-path)) (= key GLFW_KEY_BACKSPACE))
         (cond
           [(sel-range)
            (sel-delete!) (dispatch-input-change!)]
           [else
            (when (> caret 0)
              (set! focus-value
                    (string-append (substring focus-value 0 (sub1 caret))
                                   (substring focus-value caret)))
              (set! caret (sub1 caret))
              (dispatch-input-change!))])]
        [(and (not (null? focus-path)) (= key GLFW_KEY_DELETE))
         (cond
           [(sel-range)
            (sel-delete!) (dispatch-input-change!)]
           [else
            (when (< caret (string-length focus-value))
              (set! focus-value
                    (string-append (substring focus-value 0 caret)
                                   (substring focus-value (add1 caret))))
              (set! caret (string-length focus-value))
              (dispatch-input-change!))])]
        [(and (not (null? focus-path)) (= key GLFW_KEY_LEFT))
         (set! caret (max 0 (sub1 caret)))]
        [(and (not (null? focus-path)) (= key GLFW_KEY_RIGHT))
         (set! caret (min (string-length focus-value) (add1 caret)))]
        [(and (not (null? focus-path)) (= key GLFW_KEY_HOME))
         (set! caret 0)]
        [(and (not (null? focus-path)) (= key GLFW_KEY_END))
         (set! caret (string-length focus-value))]
        [(and (not (null? focus-path)) ctrl? (= key GLFW_KEY_C))
         (pw-clipboard-set! pw focus-value)]
        [(and (not (null? focus-path)) ctrl? (= key GLFW_KEY_X))
         (pw-clipboard-set! pw focus-value)
         (backspace!)]
        [(and (not (null? focus-path)) ctrl? (= key GLFW_KEY_V))
         (define clip (pw-clipboard-get pw))
         (when (and clip (non-empty-string? clip))
           (insert-at-caret! clip)
           (dispatch-input-change!))]
        [(and (not (null? focus-path)) ctrl? (= key GLFW_KEY_A))
         (set! sel-anchor 0)
         (set! caret (string-length focus-value))]
        [else (void)])))

  (define (dispatch-input-change!)
    (define l
      (and laid-root (not (null? focus-path)) (path->laid laid-root focus-path)))
    (when (and l (eq? (node-kind (laid-node l)) 'input))
      (define cb (node-prop (laid-node l) 'on-change))
      (when cb (send! (cb focus-value)))))

  (define (insert-at-caret! str)
    (set! focus-value
          (string-append (substring focus-value 0 caret)
                         str
                         (substring focus-value caret)))
    (set! caret (+ caret (string-length str))))

  (define (handle-char! cp)
(when (and (not (null? focus-path)) laid-root (>= cp 32))
      (define l (path->laid laid-root focus-path))
      (when (and l (eq? (node-kind (laid-node l)) 'input))
        (set! focus-value
              (string-append (substring focus-value 0 caret)
                             (string (integer->char cp))
                             (substring focus-value caret)))
        (set! caret (add1 caret))
        (dispatch-input-change!))))

  (define (handle-char-insert! cp)
    (void))

  ;; ---- frame ---------------------------------------------------------------------

  (define (draw-frame!)
    (theme-current theme)
    (define-values (fbw fbh) (pw-framebuffer-size pw))
    (define-values (ww wh) (pw-window-size pw))
    (define scale (/ fbw (max 1 ww)))
    (define ctx (make-font-ctx scale))
    (set! last-ctx ctx)

    (renderer-begin-frame! renderer fbw fbh scale)
    (renderer-clear! renderer (theme-bg theme))

    (set! laid-root #f)
    (define root (view state))
    (when root
      (set! laid-root (layout-view ctx root ww wh))
      (define hit (and laid-root (hit-interactive laid-root mx my)))
      (parameterize ([current-hover-path (if hit (laid-path hit) '())]
                     [current-focus-path focus-path])
        ;; cursor feedback: text fields get an I-beam, buttons a hand
        (if hit
            (pw-cursor! pw
                        (match (node-kind (laid-node hit))
                          ['input 'ibeam]
                          [_ 'hand]))
            (pw-cursor! pw 'arrow))
        (draw-laid! renderer laid-root ctx)))

    (renderer-end-frame! renderer)
    (pw-swap! pw)
    (unless shown?
      (pw-show! pw)
      (set! shown? #t)))

  ;; ---- main loop -------------------------------------------------------------------

  (let main-loop ([last-t (glfwGetTime)])
    (draw-frame!)

    (let drain ()
      (define m (async-channel-try-get msg-channel))
      (when m
        (match m
          ['(tessera:quit) (set! quit? #t)]
          [_ (set! state (update state m))])
        (drain)))

    (when on-frame
      (set! state (on-frame state 0.016)))

    (poll-events!)
    (for ([e (in-list (reverse events))])
      (match e
        [(list 'click button action)
         (handle-click! action)]
        [(list 'wheel xdy ydy)
         (handle-wheel! ydy)]
        [(list 'key key action mods)
         (handle-key! key action mods)]
        [(list 'char cp)
         (handle-char! cp)]
        [_ (void)]))
    (set! events '())

    ;; accept app-driven value changes for the focused input
    (when (and laid-root (not (null? focus-path)))
      (define l (path->laid laid-root focus-path))
      (when (and l (eq? (node-kind (laid-node l)) 'input))
        (define v (node-prop (laid-node l) 'value))
        (unless (string=? v focus-value)
          (set! focus-value v)
          (set! caret (string-length v)))))

    (cond
      [(or quit? (pw-should-close? pw))
       (close-platform-window! pw)
       state]
      [else
       (main-loop (glfwGetTime))])))
