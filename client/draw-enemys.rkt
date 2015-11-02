#lang racket
(require pict3d "current-roll-and-pos.rkt" "structures.rkt" "variables.rkt")
(provide draw-enemys draw-enemy)

(define (draw-enemys l t)
  (cond
    [(empty? l)
     empty-pict3d]
    [else
     (combine
      (draw-enemy (first l) t)
      (draw-enemys (rest l) t))]))

(define (draw-enemy o t)
  (define cp (current-pos o t))
  (combine
   (set-emitted
    (sphere (pos+ cp (dir-scale (orb-dir o) ORB-RADIUS)) (/ ORB-RADIUS 2))
    (emitted (orb-color o) 2))
   (sphere cp ORB-RADIUS)))