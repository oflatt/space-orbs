#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "current-roll-and-pos.rkt" "variables.rkt" "landscape.rkt" "shots.rkt" "draw-enemys.rkt" "scores-and-more.rkt")
(provide on-draw draw-enemy)

(define (on-draw g n ot)
  ;;(println (game-player-team g))
  (define t (- ot MASTER-TIME-OFFSET))
  (define p (game-player g))
  (combine
   (draw-dashboard g t)
   FINAL-LANDSCAPE
   (apply combine (shot-pics g t))
   (draw-enemys (get-other-orbs g) t)
   (lights+camera (current-pos p t) (orb-dir p) (current-roll p t))));;from first person perspective

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