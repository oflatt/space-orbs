#lang racket
(require pict3d "current-roll-and-pos.rkt" "structures.rkt" "variables.rkt")
(provide draw-enemys draw-enemy get-other-orbs)

;;list of orbs -> pict
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

;;game -> list of orbs
(define (get-other-orbs g)
  (get-orbs-from-teams (cons (game-player-team g) (game-enemy-teams g))))

;;list of teams -> list of orbs
(define (get-orbs-from-teams l)
  (cond
    [(empty? l)
     empty]
    [else
     (append (team-members (first l))
             (get-orbs-from-teams (rest l)))]))