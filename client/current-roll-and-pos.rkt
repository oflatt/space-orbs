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
                                         [movekeys (q-movekey STARTING-SPEED)])
                            6)
               (- (* ROTATION-SPEED-MULTIPLIER STARTING-SPEED))))
(module+ test (check-equal?
               (current-roll (struct-copy orb TESTORB
                                         [movekeys empty-movekeys])
                            6)
               0))

(define (adjust-ang ang ms dt)
  (adjust-one-ang-key ang ms dt))

(define (adjust-one-ang-key ang m dt)
  (match m
    [(movekeys w a space e)
     (+ ang (* dt e ROTATION-SPEED-MULTIPLIER))]))

;;takes an orb and time and gives current position of the orb
(define (current-pos o t)
  (adjust-pos (orb-movekeys o) (orb-dir o) (orb-pos o) (- t (orb-time o)) t (current-roll o t)))

(module+ test;;note: these tests rely on the fact that FINAL-LANDSCAPE is empty in the middle of the room.
  (check-equal?
   (current-pos TESTORB 8)
   (pos 1 1 1))
  (check-equal?
   (current-pos (struct-copy orb TESTORB
                             [movekeys (w-movekey 1/2)]
                             [pos (pos 25 21 21)]
                             [time 5]) 7)
   (pos 24 21 21))
  (check-equal?
   (current-pos (struct-copy orb TESTORB
                             [movekeys (s-movekey 1/2)]
                             [pos (pos 25 21 21)]
                             [time 5]) 7)
   (pos 26 21 21))
  (check-equal?
   (round-pos
    (current-pos (struct-copy orb TESTORB
                              [movekeys (a-movekey 1/2)]
                              [pos (pos 21 21 21)]
                              [time 5]) 7))
   (pos 21 20 21))
  (check-equal?
   (round-pos
    (current-pos (struct-copy orb TESTORB
                              [movekeys (d-movekey 1/2)]
                              [pos (pos 21 21 21)]
                              [time 5]) 7))
   (pos 21 22 21))
  (check-equal?
   (round-pos
    (current-pos (struct-copy orb TESTORB
                              [movekeys (space-movekey 1/2)]
                              [pos (pos 21 21 21)]
                              [time 5]) 7))
   (pos 21 21 22))
  (check-equal?
   (round-pos
    (current-pos (struct-copy orb TESTORB
                              [movekeys (shift-movekey 1/2)]
                              [pos (pos 21 21 21)]
                              [time 5]) 7))
   (pos 21 21 20)))

;;pos, list of movekeys, a dir, deltatime, and time-> ajusted position of the orb for the axis
(define (adjust-pos ms d p dt t ang)
  (adjust-one-key ms d p dt ang))

;; movekeys, dir, and angle -> velocity
;; pd is d adjusted by 90 degrees for pitch, and yd is adjusted 90 degrees for
;; yaw, and then they are adjusted for the angle the camera is turned
(define (key-velocity mk d ang)
  (define-values (yaw pitch) (dir->angles d))
  (define pd (rotate-around-dir (rotate-up d) d ang))
  (define yd (angles->dir (+ 90 yaw) ang))
  (match mk
    [(movekeys w a space e)
     (foldl dir+ zero-dir
            (list
             (dir-scale d w)
             (dir-scale yd a)
             (dir-scale pd space)))]))

;;movekey, dir, pos, delta-time, angle -> pos
(define (adjust-one-key mk d p dt ang)
  (match mk
    [(movekeys 0 0 0 0)
     p]
    [_
     (move-with-collision*
      p
      (key-velocity mk d ang)
      dt
      FINAL-LANDSCAPE)]))
