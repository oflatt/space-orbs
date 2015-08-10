#lang racket
(require pict3d "current-roll-and-pos.rkt" "variables.rkt")
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
  (sphere (current-pos o t) ORB-RADIUS))