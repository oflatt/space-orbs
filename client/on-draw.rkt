#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "current-roll-and-pos.rkt" "variables.rkt" "landscape.rkt" "shots.rkt")
(provide on-draw)

(define MAX-SCREEN 'beginning);maximize only works if the screen has already been drawn once

(define (on-draw g n ot)
  (define t (- ot MASTER-TIME-OFFSET))
  (on-orbs-draw (game-orbs g) n t))

(define (on-orbs-draw os n t)
  (define p (orbs-player os))
  (define draw
    (combine
     FINAL-LANDSCAPE
     (apply combine (shot-pics os t))
     (draw-enemys (orbs-enemys os) t)
     (lights+camera (current-pos p t) (orb-dir p) (current-roll p t))))
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

(define (draw-enemys l t)
  (cond
    [(empty? l)
     empty-pict3d]
    [else
     (combine
      (draw-enemy (first l) t)
      (draw-enemys (rest l) t))]))

(define (draw-enemy o t)
  (sphere (current-pos o t) ORB-RADIUS))

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