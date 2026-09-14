#lang racket/base

;; Widget gallery: every control in one window.

(require racket/match
         racket/string
         racket/list
         racket/bool
         tessera)

(define (view st)
  (match-define (list checked slider-val progress-val tab) st)
  (column
   #:padding 24
   #:spacing 14
   (text "控件一览" #:size 20)
   (divider)
   (row #:spacing 10
        (button "主要" #:on-click (λ () 'noop))
        (button "次要" #:kind 'secondary #:on-click (λ () 'noop))
        (button "幽灵" #:kind 'ghost #:on-click (λ () 'noop))
        (button "危险" #:kind 'danger #:on-click (λ () 'noop)))
   (checkbox "我已阅读并同意条款" #:checked? checked
             #:on-change (λ (v) (list 'checked v)))
   (row #:spacing 10 #:align 'center
        (text "进度" #:color (theme-text-muted (theme-current)))
        (progress progress-val))
   (row #:spacing 10
        (text "标签页" #:color (theme-text-muted (theme-current)))
        (button "概览" #:kind (if (eq? tab 'overview) 'primary 'secondary)
                #:on-click (λ () (list 'tab 'overview)))
        (button "明细" #:kind (if (eq? tab 'detail) 'primary 'secondary)
                #:on-click (λ () (list 'tab 'detail))))
   (box #:bg (theme-surface (theme-current)) #:radius 10
        (column
         (text (match tab
                 ['overview "概览：这里显示汇总信息。"]
                 ['detail "明细：这里显示全部数据。"])
               #:color (theme-text-muted (theme-current))))))
  )

(module+ main
  (run #:title "控件一览"
       #:width 560 #:height 380
       #:init-state (list #f 0.4 0.65 'overview)
       #:update
       (λ (s m)
         (match m
           [(list 'checked v) (list v (second s) (third s) (fourth s))]
           [(list 'tab v) (list (first s) (second s) (third s) v)]
           [else s]))
       #:view view))
