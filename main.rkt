#lang racket/base

;; tessera — GPU-accelerated cross-platform UI toolkit for Racket.
;;
;; One `require` gives the whole public API:
;;
;;   (require tessera)
;;   (run #:title "Hello"
;;        #:init-state 0
;;        #:update (λ (s m) (match m ['inc (add1 s)] [else s]))
;;        #:view   (λ (s) (column (text (format "~a" s))
;;                                (button "＋1" #:on-click (λ () 'inc)))))
;;
;; Lower layers (tessera/render, tessera/text, tessera/platform, ...) are
;; separate modules for advanced use; see the manual.

(require tessera/render
         tessera/theme
         tessera/view
         tessera/layout
         tessera/text
         tessera/app
         tessera/image
         tessera/snapshot)

(provide
 ;; application
 run
 post!
 quit!
 ;; views
 (struct-out node)
 node-prop
 text row column spacer box button checkbox progress input divider
 ;; theming
 (struct-out theme)
 theme:light theme:dark theme-current
 ;; colors
 (struct-out color) color-hex color-argb color-scale
 ;; text utilities
 find-font-file make-font-set text-width wrap-text draw-text!
 ;; snapshots (headless rendering for tests)
 render-view->png
 ;; images
 png-write qoi-encode qoi-decode bmp-decode tga-decode image-load)

;; theme-current is a parameter — provide it through a wrapper-safe form
;; (parameters are values; this re-export keeps (theme-current) callable).
(provide (rename-out [theme-current current-theme]))

;; draw-text!/text-width expect a laid context font-set; power users can
;; build one directly.
(provide font-units->px glyph-outline glyph-advance load-font)
(require tessera/font)
