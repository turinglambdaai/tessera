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
;; into state. Interaction bookkeeping (hover, focus, caret) lives here,
;; keyed by stable tree paths.

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

  ;; per-scale font provider: primary face + CJK fallback face
  (define (make-font-ctx scale)
    (make-ui-ctx
     (lambda (pt) (make-font-set font-file (exact-round (* pt scale))))
     (and cjk-font-file
          (lambda (pt) (make-font-set cjk-font-file (exact-round (* pt scale)))))
     scale))

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
   (lambda (w x y) (set! mx x) (set! my y)))
  (glfwSetKeyCallback
   win
   (lambda (w key scancode action mods)
     (unless (zero? action)
       (push-event! (list 'key key action mods)))))
  (glfwSetCharCallback
   win
   (lambda (w cp) (push-event! (list 'char cp))))

  ;; ---- input handling ------------------------------------------------------------

  ;; Dispatch a click at the current cursor position against the laid tree.
  (define (handle-click!)
    (define hit (and laid-root (hit-interactive laid-root mx my)))
    (unless hit
      (set! focus-path '())
      (set! focus-value "")
      (set! caret 0))
    (when hit
      (define n (laid-node hit))
      (match (node-kind n)
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
           (set! caret (string-length focus-value)))]
        [_ (void)])))

  (define (insert-at-caret! str)
    (set! focus-value
          (string-append (substring focus-value 0 caret)
                         str
                         (substring focus-value caret)))
    (set! caret (+ caret (string-length str))))

  (define (backspace!)
    (when (> caret 0)
      (set! focus-value
            (string-append (substring focus-value 0 (sub1 caret))
                           (substring focus-value caret)))
      (set! caret (sub1 caret))
      (define l (and laid-root (path->laid laid-root focus-path)))
      (when l
        (define cb (node-prop (laid-node l) 'on-change))
        (when cb (send! (cb focus-value))))))

  (define (handle-key! key action mods)
    (define ctrl? (not (zero? (bitwise-and mods GLFW_MOD_CONTROL))))
    (define super? (not (zero? (bitwise-and mods GLFW_MOD_SUPER))))
    (define alt? (not (zero? (bitwise-and mods GLFW_MOD_ALT))))
    (when (= action GLFW_PRESS)
      (when (and (= key (char->integer (char-upcase quit-key)))
                 (match quit-mods
                   ['super (or super? ctrl?)]
                   ['control ctrl?]
                   ['alt alt?]
                   [#f #f]))
        (set! quit? #t))
      (when (and (not (null? focus-path)) laid-root)
        (define l (path->laid laid-root focus-path))
        (when (and l (eq? (node-kind (laid-node l)) 'input))
          (match key
            [(== GLFW_KEY_BACKSPACE)
             (backspace!)]
            [(== GLFW_KEY_DELETE)
             (when (< caret (string-length focus-value))
               (set! focus-value
                     (string-append (substring focus-value 0 caret)
                                    (substring focus-value (add1 caret))))
               (set! caret (string-length focus-value))
               (define cb (node-prop (laid-node l) 'on-change))
               (when cb (send! (cb focus-value))))]
            [(== GLFW_KEY_LEFT)
             (set! caret (max 0 (sub1 caret)))]
            [(== GLFW_KEY_RIGHT)
             (set! caret (min (string-length focus-value) (add1 caret)))]
            [(== GLFW_KEY_HOME)
             (set! caret 0)]
            [(== GLFW_KEY_END)
             (set! caret (string-length focus-value))]
            [(== GLFW_KEY_C)
             #:when ctrl?
             (pw-clipboard-set! pw focus-value)]
            [(== GLFW_KEY_X)
             #:when ctrl?
             (pw-clipboard-set! pw focus-value)
             (backspace!)]
            [(== GLFW_KEY_V)
             #:when ctrl?
             (define clip (pw-clipboard-get pw))
             (when (and clip (non-empty-string? clip))
               (set! focus-value
                     (string-append (substring focus-value 0 caret)
                                    clip
                                    (substring focus-value caret)))
               (set! caret (+ caret (string-length clip)))
               (define cb (node-prop (laid-node l) 'on-change))
               (when cb (send! (cb focus-value))))]
            [_ (void)])))))

  (define (handle-char! cp)
    (when (and (not (null? focus-path)) laid-root (>= cp 32))
      (define l (path->laid laid-root focus-path))
      (when (and l (eq? (node-kind (laid-node l)) 'input))
        (set! focus-value
              (string-append (substring focus-value 0 caret)
                             (string (integer->char cp))
                             (substring focus-value caret)))
        (set! caret (add1 caret))
        (define cb (node-prop (laid-node l) 'on-change))
        (when cb (send! (cb focus-value))))))

  ;; ---- frame ---------------------------------------------------------------------

  (define (draw-frame!)
    (theme-current theme)
    (define-values (fbw fbh) (pw-framebuffer-size pw))
    (define-values (ww wh) (pw-window-size pw))
    (define scale (/ fbw (max 1 ww)))

    (renderer-begin-frame! renderer fbw fbh scale)
    (renderer-clear! renderer (theme-bg theme))

    (set! laid-root #f)
    (define root (view state))
    (when root
      (define ctx (make-font-ctx scale))
      (set! laid-root (layout-view ctx root ww wh))
      (parameterize ([current-hover-path
                      (let ([hit (and laid-root (hit-interactive laid-root mx my))])
                        (if hit (laid-path hit) '()))]
                     [current-focus-path focus-path])
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
         (when (= action GLFW_PRESS)
           (handle-click!))]
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
