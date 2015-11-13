#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "current-roll-and-pos.rkt" "variables.rkt" "landscape.rkt" "shots.rkt" "draw-enemys.rkt" "scores-and-more.rkt")
(provide on-draw draw-enemy)

(define (on-draw g n ot)
  (define t (- ot MASTER-TIME-OFFSET))
  (combine
   (draw-scores g t)
   (on-orbs-draw (game-orbs g) n t)))

(define (on-orbs-draw os n t)
  (define p (orbs-player os))
  (define draw
    (combine
     FINAL-LANDSCAPE
     (make-cross p t)
     (apply combine (shot-pics os t))
     (draw-enemys (orbs-enemys os) t)
     (lights+camera (current-pos p t) (orb-dir p) (current-roll p t))))
  draw)

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