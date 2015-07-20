#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "landscape.rkt" "current-roll-and-pos.rkt" "rotate-dir.rkt" "variables.rkt")
(provide on-mouse)

(define BEGINNING? #t);sets the mous to the middle initially without moving the view

(define (on-mouse g n t x-ignored y-ignored e)
  (cond
    [BEGINNING?
     (set! BEGINNING? #f)
     (move-mouse-to-center)
     g]
    [else
     (struct-copy game g
                  [orbs (on-orbs-mouse (game-orbs g) n t e)])]))
                  

(define (on-orbs-mouse os n t e)
  (struct-copy orbs os
               [player (on-player-mouse (orbs-player os) n t e)]))

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

(define (new-shot o poc t)
  (define cp (current-pos o t))
  (define-values (yaw pitch) (dir->angles (orb-dir o)))
  (define center (pos-between (current-pos o t) poc 1/2))
  (define h (/ SHOT-WIDTH 2))
  (shot
   (move
    (rotate-z
     (rotate-y
      (ellipsoid
       (pos (- (- (/ (pos-dist cp poc) 2) 1)) (- h) (- h))
       (pos (- (/ (pos-dist cp poc) 2) 1) h h))
      (- pitch))
     yaw)
    (dir (pos-x center) (pos-y center) (pos-z center)))
   t))

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
      (- (dir-to-roll new-dir pd) 90))]))

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
      (dir-to-roll new-dir pd))]))