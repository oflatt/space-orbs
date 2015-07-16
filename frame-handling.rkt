#lang racket/base
(require racket/gui/base
         racket/class)

(provide get-mouse-delta maximize-screen)

(define (get-mouse-delta)
  (define f (get-top-level-focus-window))
  (cond
    [(not f) (values 0 0)]
    [else
     (define-values (pos mods)
       (get-current-mouse-state))
     (define-values (w h) (get-display-size))
     (define-values (adjust-x adjust-y)
       (cond
         [(eq? (system-type) 'macosx)
          (get-display-left-top-inset)]
         [else (values 0 0)]))
     (define cx (quotient w 2))
     (define cy (quotient h 2))
     (define-values (wcx wcy) (send f screen->client (+ cx adjust-x) (+ cy adjust-y)))
     (send f warp-pointer wcx wcy)
     (values (- (send pos get-x) cx)
             (- (send pos get-y) cy))]))

(module+ test
  (define f (new frame%
                 [label "Test"]
                 [width 300]
                 [height 300]))
  (send f show #t)
  (let loop ()
    (sleep/yield 0.1)
    (define-values (dx dy) (get-mouse-delta))
    (printf "~s ~s\n" dx dy)
    (loop)))

(define (maximize-screen)
  (define f (get-top-level-focus-window))
  (send f maximize #t))