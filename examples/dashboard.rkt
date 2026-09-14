#lang racket/base

;; A small analytics dashboard: cards, bars, and a live clock feel.

(require racket/match
         racket/string
         racket/list
         racket/bool
         tessera)

(define (bar-row label frac color)
  (row #:spacing 10 #:align 'center
       (box #:width 56
            (text label #:size 12
                  #:color (theme-text-muted (theme-current))))
       (progress frac #:height 10 #:color color)))

(define (view st)
  (match-define (list revenue orders conv pending) st)
  (column
   #:padding 24
   #:spacing 16
   (row #:align 'center
        (text "运营看板" #:size 20)
        (spacer)
        (text "今日" #:size 12 #:color (theme-text-muted (theme-current))))
   (row #:spacing 12
        (box #:padding 16 #:bg (theme-surface (theme-current)) #:radius 10
             (column
              (text "营收" #:size 12 #:color (theme-text-muted (theme-current)))
              (text (format "¥~a" revenue) #:size 22)))
        (box #:padding 16 #:bg (theme-surface (theme-current)) #:radius 10
             (column
              (text "订单" #:size 12 #:color (theme-text-muted (theme-current)))
              (text (number->string orders) #:size 22)))
        (box #:padding 16 #:bg (theme-surface (theme-current)) #:radius 10
             (column
              (text "转化率" #:size 12 #:color (theme-text-muted (theme-current)))
              (text (format "~a%" conv) #:size 22))))
   (box #:padding 16 #:bg (theme-surface (theme-current)) #:radius 10
        (column
         #:spacing 10
         (text "渠道分布" #:size 13)
         (bar-row "直营" 0.62 (theme-accent (theme-current)))
         (bar-row "渠道A" 0.41 (theme-success (theme-current)))
         (bar-row "渠道B" 0.23 (theme-warning (theme-current)))))
   (button "标记全部已处理"
           #:kind 'secondary
           #:enabled? (> pending 0)
           #:on-click (λ () 'clear-pending))))

(module+ main
  (run #:title "运营看板"
       #:width 720 #:height 480
       #:init-state (list 128430 372 "3.8" 12)
       #:update (λ (s m) (match m ['clear-pending (list (first s) (second s) (third s) 0)] [else s]))
       #:view view))
