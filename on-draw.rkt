#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "frame-handling.rkt" "structures.rkt" "current-ang-and-pos.rkt" "variables.rkt" "landscape.rkt")
(provide on-draw)

(define MAX-SCREEN? 'beginning);we have to go through this because maximize only works if the screen has already been drawn once

(define (on-draw os n t)
  (define p (orbs-player os))
  (define draw
    (combine
     FINAL-LANDSCAPE
     (apply combine (list-of-shots os))
     (draw-enemy (orbs-enemy os) t)
     (lights+camera (current-pos p t) (orb-dir p) (current-ang p t))))
  (cond
    [(equal? MAX-SCREEN? 'beginning)
     (set! MAX-SCREEN? #t)
     draw]
    [MAX-SCREEN?
     (maximize-screen)
     draw]
    [else draw]))

(define (list-of-shots os)
  (append
   (orb-shots (orbs-player os))
   (orb-shots (orbs-enemy os))))

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