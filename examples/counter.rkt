#lang racket/base

;; The smallest useful tessera app: a click counter.

(require racket/match
         racket/string
         racket/list
         racket/bool
         tessera)

(define (view count)
  (column
   #:spacing 16
   #:align 'center
   (text (format "点击次数：~a" count) #:size 28)
   (row #:spacing 12
        (button "＋1" #:on-click (λ () 'inc))
        (button "重置" #:kind 'secondary
                #:on-click (λ () 'reset)))))

(module+ main
  (run #:title "计数器"
       #:width 360 #:height 220
       #:init-state 0
       #:update (λ (s m) (match m ['inc (add1 s)] ['reset 0] [else s]))
       #:view view))
