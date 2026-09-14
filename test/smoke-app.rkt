#lang racket/base

;; End-to-end test of the run loop: counter app with a self-terminating
;; timer. Verifies event dispatch, message flow, and clean shutdown.

(require rackunit
         racket/match
         racket/async-channel
         tessera)

(define frames-ht (make-hash))          ; 'n -> frame count

(module+ test
  (define final
    (run #:title "tessera-run-test"
         #:width 320 #:height 200
         #:init-state 0
         #:update (λ (s m)
                    (match m
                      ['inc (add1 s)]
                      [(list 'set v) v]
                      [else s]))
         #:view
         (λ (s)
           (hash-set! frames-ht 'n (add1 (hash-ref frames-ht 'n 0)))
           ;; post messages from "user land" on the early frames
           (when (= (hash-ref frames-ht 'n) 2) (post! 'inc))
           (when (= (hash-ref frames-ht 'n) 4) (post! '(set 42)))
           (when (>= (hash-ref frames-ht 'n) 8) (quit!))
           (column
            (text (format "count = ~a" s) #:size 20)))
         #:on-frame (λ (s dt) s)))
  (check-equal? final 42 "run loop folded posted messages into final state")
  (check-true (>= (hash-ref frames-ht 'n) 8) "ran at least 8 frames"))
