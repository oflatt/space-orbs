#lang racket
(require pict3d pict3d-die-cut lens "structures.rkt" "current-roll-and-pos.rkt")
(provide make-cross draw-scores)

;;orb and time-> pict
;;draws little cursor thing
(define (make-cross o t)
  (define l (/ 1.5 16))
  (define h (/ l 7))
  (define hl (/ l 2))
  (define hh (/ h 2))
  (define gaph (/ l 6))
  (define half-cross
    (combine
      (rectangle
       (pos gaph (- hh) (- hh))
       (pos (+ hl h) hh hh));;right
      (rectangle
       (pos (- gaph) (- hh) (- hh))
       (pos (- (- hl) h) hh hh))));;left
  (define cross-pict
    (combine
     (rotate-x
      half-cross
      45)
     (rotate-z
      (rotate-y
       half-cross
       90)
      45)))
  (put-in-front o t cross-pict))

;;orb and time -> pict
;;if tab is 
(define (draw-scores g t)
  (define o
    (lens-view
     game-orbs-player-lens
     g))
  (cond
    [(game-scores? g)
     (put-in-front
      o
      t
      (make-scoreboard o t))]
    [else
     empty-pict3d]))

(define (make-scoreboard o t)
  (move-z
   (move-y
    (rotate-z
     (scale
     (die-cut-text "KILLS" #:expected-scale 0.1 #:depth 8 #:top? true #:bottom? true #:sides? true)
     0.5)
     90)
    0)
   20))

;;takes an orb, time, and a pict. Orients the pic in front of the orb
;;this assumes the pict is centered around origin
(define (put-in-front o t p)
  (define cp (current-pos o t))
  (define pict-pos (pos+ cp (dir-scale (orb-dir o) 1)))
  (define-values (yaw pitch) (dir->angles (orb-dir o)))
  (move
   (rotate
    (rotate-z
     (rotate-x
      p
      (- pitch))
     (+ yaw 90))
    (orb-dir o)
    (current-roll o t))
   (pos->dir pict-pos)))