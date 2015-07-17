#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "landscape.rkt" "current-ang-and-pos.rkt" "rotate-dir.rkt" "variables.rkt")
(provide on-mouse)

(define BEGINNING? #t);sets the mous to the middle initially without moving the view

(define (on-mouse os n t x-ignored y-ignored e)
  (cond
    [BEGINNING?
     (set! BEGINNING? #f)
     (move-mouse-to-center)
     os]
    [else
     (struct-copy orbs os
                  [player (on-player-mouse (orbs-player os) n t e)]
                  [enemy (orbs-enemy os)])]))

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
         [else (cons (rectangle (current-pos o t) poc) (orb-shots o))])])];;FIXME: this is what the player's shots look like
    [else
     (struct-copy
      orb
      o
      [pos (current-pos o t)]
      [time t]
      [dir (first n)]
      [ang (second n)])]))

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
    (orb-dir o)
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
  (define dir (first l))
  (define d (rotation-to-dir dir (second l)))
  (define pd (rotate-around-dir dir d 90))
  (cond
    [(equal? y 0)
     l]
    [else
     (define new-dir
       (rotate-around-dir pd
                          dir
                          (/ (- y) 2)))
     (list
      new-dir
      ;; Find the new angle to keep pd the same next time:
      (- (dir-to-rotation new-dir pd) 90))]))

;;yaw is rotation about z axis, so we compare x positions
;;gives a list of dir and ang
(define (adjust-for-mouse-x dir o x y t)
  (define pd (rotation-to-dir (orb-dir o) (current-ang o t)))
  (cond
    [(equal? x 0)
     (list
      dir
      (current-ang o t))]
    [else
     (define new-dir
       (rotate-around-dir pd
                          dir
                          (/ (- x) 2)))
     (list
      new-dir
      ;; Find the new angle to keep pd the same next time:
      (dir-to-rotation new-dir pd))]))