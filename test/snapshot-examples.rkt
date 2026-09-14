#lang racket/base

;; Snapshot the example apps' views to PNG (headless rendering).

(require rackunit
         racket/file
         racket/list
         racket/match
         racket/bool
         tessera/snapshot
         tessera/theme
         tessera/view)

(define out (build-path "snapshots"))
(make-directory* out)

;; --- counter ---
(define (counter-view count)
  (column
   #:spacing 16
   #:align 'center
   (text (format "点击次数：~a" count) #:size 28)
   (row #:spacing 12
        (button "＋1" #:on-click (λ () 'inc))
        (button "重置" #:kind 'secondary #:on-click (λ () 'reset)))))

;; --- dashboard ---
(define (dash-view)
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
              (text "¥128430" #:size 22)))
        (box #:padding 16 #:bg (theme-surface (theme-current)) #:radius 10
             (column
              (text "订单" #:size 12 #:color (theme-text-muted (theme-current)))
              (text "372" #:size 22))))
   (box #:padding 16 #:bg (theme-surface (theme-current)) #:radius 10
        (column
         (text "渠道分布" #:size 13)
         (row #:spacing 10 #:align 'center
              (box #:width 56 (text "直营" #:size 12 #:color (theme-text-muted (theme-current))))
              (progress 0.62 #:height 10 #:color (theme-accent (theme-current)))))
        )
   (button "标记全部已处理" #:kind 'secondary)))

;; render both themes
(render-view->png (build-path out "example-counter.png")
                  (counter-view 3)
                  #:width 360 #:height 200 #:scale 2)
(render-view->png (build-path out "example-dashboard.png")
                  (dash-view)
                  #:width 640 #:height 360 #:scale 2)
(parameterize ([theme-current theme:dark])
  (render-view->png (build-path out "example-dashboard-dark.png")
                    (λ () (dash-view))
                    #:width 640 #:height 360
                    #:theme theme:dark
                    #:scale 2))

;; snapshots are real PNGs
(define png1 (file->bytes (build-path out "example-counter.png")))
(check-equal? (subbytes png1 0 8) (bytes 137 80 78 71 13 10 26 10))
