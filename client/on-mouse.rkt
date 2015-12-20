#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "landscape.rkt" "current-roll-and-pos.rkt" "rotate-dir.rkt" "variables.rkt" "shots.rkt" "on-frame.rkt")
(provide on-mouse)

(define BEGINNING? #t);sets the mouse to the middle initially without moving the view

(define (on-mouse g n ot x-ignored y-ignored e)
  (define t (- ot MASTER-TIME-OFFSET));;all the functions used in big-bang need this
  (cond
    [BEGINNING?
     (set! BEGINNING? #f)
     (move-mouse-to-center)
     g]
    [(equal? e "left-down")
     (on-shoot (update-mt g t) t)]
    [else
     (struct-copy game g
                  [player (on-player-mouse (game-player g) n t e)]
                  [mt t])]))

;;orb -> orb
(define (on-player-mouse p n t e)
  (define result
    (on-player-mouse-helper p n t e))
  (unless (equal? result p)
    (send-orb* result e t))
  result)

(define (on-player-mouse-helper o n t e)
  (define-values (x y) (get-mouse-delta))
  (define x-scaled
    (scale-for-sensitivity x))
  (define y-scaled
    (scale-for-sensitivity y))
  (define n (new-dir-and-ang o x-scaled y-scaled t))
  (struct-copy
   orb
   o
   [pos (current-pos o t)]
   [time t]
   [dir (first n)]
   [roll (second n)]))

(define (scale-for-sensitivity unknown-x)
  (define negateorone
    (cond
      [(negative? unknown-x)
       -1]
      [else
       1]));;-1 or 1, -1 if unknown-x was negative
  (define x
    (cond
      [(equal? negateorone -1)
       (abs unknown-x)]
      [else
       unknown-x]))
  (cond
    [USE-MOUSE-CURVE?
     (*
      MOUSE-SENSITIVITY
      (cond
        [(> x MOUSE-MOVE-WAVE-MAX)
         (- x 20)]
        [else
         (* negateorone
            (* MOUSE-MOVE-WAVE-MAX
               (- 1
                  (cos (* (/ x MOUSE-MOVE-WAVE-MAX) (/ pi 2))))))]))]
    [else (* unknown-x MOUSE-SENSITIVITY)]))

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