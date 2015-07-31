#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "landscape.rkt" "current-roll-and-pos.rkt" "rotate-dir.rkt" "variables.rkt" "shots.rkt" "on-frame.rkt")
(provide on-mouse)

(define BEGINNING? #t);sets the mouse to the middle initially without moving the view

(define (on-mouse g n ot x-ignored y-ignored e)
  (define t (- ot MASTER-TIME-OFFSET))
  (cond
    [BEGINNING?
     (set! BEGINNING? #f)
     (move-mouse-to-center)
     g]
    [else
     (struct-copy game g
                  [orbs (on-orbs-mouse (game-orbs g) n t e)]
                  [mt t])]))


(define (on-orbs-mouse os n t e)
  (define result
    (struct-copy orbs os
                 [player (on-player-mouse (orbs-player os) n t e)]))
  (send-orb* (orbs-player result))
  result)

(define (on-player-mouse o n t e)
  (define-values (x y) (get-mouse-delta))
  (define n (new-dir-and-ang o x y t))
  (cond
    [(equal? e "left-down")
     (define poc (trace FINAL-LANDSCAPE (current-pos o t) (orb-dir o)))
     (struct-copy
      orb
      o
      [shots
       (cond
         [(equal? poc #f)
          empty]
         [else (cons (new-shot o poc t) (orb-shots o))])])]
    [else
     (struct-copy
      orb
      o
      [pos (current-pos o t)]
      [time t]
      [dir (first n)]
      [roll (second n)])]))

(module+ test
  (check-equal?
   (round-orbs-dir
    (on-player-mouse
     TESTORB
     1
     5
     "drag"))
   (struct-copy
    orb
    TESTORB
    [dir -x])))

(define (new-dir-and-ang o x y t)
  (define-values (oldyaw oldpitch) (dir->angles (orb-dir o)))
  (adjust-for-mouse-y
   (adjust-for-mouse-x
    o
    x
    y
    t)
   o
   x
   y
   t))

;;takes list of a dir and ang and o x y t-> list of dir and ang
(define (adjust-for-mouse-y l o x y t)
  (define d (first l))
  (define pd (rotate-right d #:roll (second l)))
  (cond
    [(equal? y 0)
     l]
    [else
     (define new-dir
       (rotate-around-dir d
                          pd
                          (/ (- y) 2)))
     (list
      new-dir
      ;; Find the new angle to keep pd the same next time:
      (- (dir-up-to-roll new-dir pd) 90))]))

;;yaw is rotation about z axis, so we compare x positions
;;gives a list of dir and ang
(define (adjust-for-mouse-x o x y t)
  (define d (orb-dir o))
  (define pd (rotate-up (orb-dir o) #:roll (current-roll o t)))
  (cond
    [(equal? x 0)
     (list
      d
      (current-roll o t))]
    [else
     (define new-dir
       (rotate-around-dir d
                          pd
                          (/ (- x) 2)))
     (list
      new-dir
      ;; Find the new roll to keep pd the same next time:
      (dir-up-to-roll new-dir pd))]))