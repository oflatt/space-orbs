#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "frame-handling.rkt" "structures.rkt" "current-ang-and-pos.rkt" "variables.rkt" "landscape.rkt")
(provide on-draw)

(define MAX-SCREEN 'beginning);we have to go through this because maximize only works if the screen has already been drawn once

(define (on-draw g n t)
  (on-orbs-draw (game-orbs g) n t))

(define (on-orbs-draw os n t)
  (define p (orbs-player os))
  (define draw
    (combine
     FINAL-LANDSCAPE
     (apply combine (list-of-shot-pictures os))
     (draw-enemy (orbs-enemy os) t)
     (lights+camera (current-pos p t) (orb-dir p) (current-ang p t))))
  (cond
    [(equal? MAX-SCREEN 'beginning)
     (set! MAX-SCREEN 'ready)
     draw]
    [(equal? MAX-SCREEN 'ready)
     (maximize-screen)
     (make-cursor-blank)
     (set! MAX-SCREEN 'done)
     draw]
    [else draw]))

(define (list-of-shot-pictures os)
  (append
   (get-pics-from-shots
    (orb-shots (orbs-player os)))
   (get-pics-from-shots
    (orb-shots (orbs-enemy os)))))

(define (get-pics-from-shots l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons
      (shot-pic (first l))
      (get-pics-from-shots (rest l)))]))

(define (draw-enemy o t)
  (sphere (current-pos o t) 1))

(define (lights+camera currentpos d ang)
  (combine (apply combine
                  (cond
                    [DISCO?
                     (pick-random-lights empty NUM-OF-LIGHTS)]
                    [else LIGHTS-LIST]))
           (basis 'camera (point-at
                           currentpos
                           d
                           #:angle ang))))