#lang racket/base

;; A login form: two controlled inputs, validation, and a status line.

(require racket/match
         racket/string
         racket/list
         racket/bool
         tessera)

;; state: (list username password status submitting)
(define (view state)
  (match-define (list user pass status submitting) state)
  (column
   #:align 'center
   (box #:padding 28 #:bg (theme-surface (theme-current)) #:radius 12
        #:width 360
        (column
         #:spacing 12
         (text "登录" #:size 22)
         (text "用户名" #:size 12 #:color (theme-text-muted (theme-current)))
         (input user
                #:placeholder "you@example.com"
                #:on-change (λ (v) (list 'user v)))
         (text "密码" #:size 12 #:color (theme-text-muted (theme-current)))
         (input pass
                #:placeholder "••••••••"
                #:password? #t
                #:on-change (λ (v) (list 'pass v)))
         (spacer 0 4)
         (button (if submitting "登录中…" "登录")
                 #:enabled? (and (non-empty-string? user)
                                 (>= (string-length pass) 4)
                                 (not submitting))
                 #:on-click (λ () 'submit))
         (when status (text status #:size 12
                            #:color (theme-text-muted (theme-current))))))))

(module+ main
  (run #:title "登录"
       #:width 520 #:height 420
       #:init-state (list "" "" #f #f)
       #:update
       (λ (s m)
         (match m
           [(list 'user v) (list v (second s) (third s) (fourth s))]
           [(list 'pass v) (list (first s) v (third s) (fourth s))]
           ['submit (list (first s) (second s) "已提交（示例）" #t)]
           [else s]))
       #:view view))
