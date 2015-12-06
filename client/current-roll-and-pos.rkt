#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "structures.rkt" "variables.rkt" "rotate-dir.rkt" "landscape.rkt" "collision-detection.rkt")
(provide current-pos current-roll)
;;this is where all the magic happens.

(define (current-roll o t)
  (define ms (orb-movekeys o))
  (adjust-ang (orb-roll o) ms (- t (orb-time o))))

(module+ test (check-equal?
               (current-roll TESTORB 50)
               0))

(module+ test (check-equal?
               (current-roll (struct-copy orb TESTORB
                                         [movekeys (list (movekey "q" STARTING-SPEED))])
                            6)
               (- (* ROTATION-SPEED-MULTIPLIER STARTING-SPEED))))
(module+ test (check-equal?
               (current-roll (struct-copy orb TESTORB
                                         [movekeys empty])
                            6)
               0))

(define (adjust-ang ang ms dt)
  (cond
    [(empty? ms)
     ang]
    [else
     (adjust-ang (adjust-one-ang-key ang (first ms) dt) (rest ms) dt)]))

(define (adjust-one-ang-key ang m dt)
  (cond
    [(equal? (movekey-key m) "q")
     (- ang (* dt (movekey-speed m) ROTATION-SPEED-MULTIPLIER))]
    [(equal? (movekey-key m) "e")
     (+ ang (* dt (movekey-speed m) ROTATION-SPEED-MULTIPLIER))]
    [else ang]))

;;takes an orb and time and gives current position of the orb
(define (current-pos o t)
  (define ms (orb-movekeys o))
  (cond
    [(equal? ms '())
     (orb-pos o)]
    [else
     (adjust-pos (orb-movekeys o) (orb-dir o) (orb-pos o) (- t (orb-time o)) t (current-roll o t))]))

(module+ test (check-equal?
               (current-pos TESTORB 8)
               (pos 1 1 1)))
(module+ test (check-equal?
               (current-pos (struct-copy orb TESTORB
                                         [movekeys (list (movekey "w" 1/2))]
                                         [pos (pos 5 1 1)]
                                         [time 5]) 7)
               (pos 4 1 1)))
(module+ test (check-equal?
               (current-pos (struct-copy orb TESTORB
                                         [movekeys (list (movekey "s" 1/2))]
                                         [pos (pos 5 1 1)]
                                         [time 5]) 7)
               (pos 6 1 1)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey "a" 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 0 1)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey "d" 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 2 1)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey " " 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 1 2)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey "shift" 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 1 0)))

;;pos, list of movekeys, a dir, deltatime, and time-> ajusted position of the orb for the axis
(define (adjust-pos ms d p dt t ang)
  (cond
    [(empty? ms)
     p]
    [else
     (adjust-pos (rest ms) d (adjust-one-key (first ms) d p dt ang) dt t ang)]))

;;movekey, dir, pos, old time, and current time -> pos
;;pd is d adjusted by 90 degrees for pitch, and yd is adjusted 90 degrees for yaw, and then they are adjusted for the angle the camera is turned
(define (adjust-one-key mk d p dt ang)
  (define-values (yaw pitch) (dir->angles d))
  (define pd (rotate-around-dir (rotate-up d) d ang))
  (define yd (angles->dir (+ 90 yaw) ang))
  (define s (movekey-speed mk))
  (cond
    [(equal? (movekey-key mk) "w")
     (move-with-collision*
      p
      (dir-scale d s)
      dt
      FINAL-LANDSCAPE)]
    [(equal? (movekey-key mk) "s")
     (move-with-collision*
      p
      (dir-scale (dir-negate d) s)
      dt
      FINAL-LANDSCAPE)]
    [(equal? (movekey-key mk) "a")
     (move-with-collision*
      p
      (dir-scale yd s)
      dt
      FINAL-LANDSCAPE)]
    [(equal? (movekey-key mk) "d")
     (move-with-collision*
      p
      (dir-scale (dir-negate yd) s)
      dt
      FINAL-LANDSCAPE)]
    [(equal? (movekey-key mk) "shift")
     (move-with-collision*
      p
      (dir-scale (dir-negate pd) s)
      dt
      FINAL-LANDSCAPE)]
    [(equal? (movekey-key mk) " ")
     (move-with-collision*
      p
      (dir-scale pd s)
      dt
      FINAL-LANDSCAPE)]
    [else p]))