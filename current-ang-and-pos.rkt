#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "structures.rkt" "variables.rkt" "rotate-dir.rkt" "landscape.rkt")
(provide current-pos current-ang)

(define (current-ang o t)
  (define ms (orb-movekeys o))
  (adjust-ang (orb-ang o) ms (- t (orb-time o))))

(module+ test (check-equal?
               (current-ang TESTORB 50)
               0))
(module+ test (check-equal?
               (current-ang (struct-copy orb TESTORB
                                         [movekeys (list (movekey "q" 1/80))])
                            6)
               (- (* ROTATION-SPEED-MULTIPLIER STARTING-SPEED))))

(define (adjust-ang ang ms at)
  (cond
    [(empty? ms)
     ang]
    [else
     (adjust-ang (adjust-one-ang-key ang (first ms) at) (rest ms) at)]))

(define (adjust-one-ang-key ang m at)
  (cond
    [(equal? (movekey-key m) "q")
     (- ang (* at (movekey-speed m) ROTATION-SPEED-MULTIPLIER))]
    [(equal? (movekey-key m) "e")
     (+ ang (* at (movekey-speed m) ROTATION-SPEED-MULTIPLIER))]
    [else ang]))

;;takes an orb and time and gives current position of the orb
(define (current-pos o t)
  (define ms (orb-movekeys o))
  (cond
    [(equal? ms '())
     (orb-pos o)]
    [else
     (adjust-pos (orb-movekeys o) (orb-dir o) (orb-pos o) (- t (orb-time o)) t (current-ang o t))]))

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
  (cond
    [(equal? (movekey-key mk) "w")
     (move-with-collision
      p
      (pos
       (+ (pos-x  p) (*  (movekey-speed mk) dt (dir-dx d)))
       (+ (pos-y  p) (*  (movekey-speed mk) dt (dir-dy d)))
       (+ (pos-z  p) (*  (movekey-speed mk) dt (dir-dz d))))
      d)]
    [(equal? (movekey-key mk) "s")
     (move-with-collision
      p
      (pos
       (- (pos-x  p) (*  (movekey-speed mk) dt (dir-dx d)))
       (- (pos-y  p) (*  (movekey-speed mk) dt (dir-dy d)))
       (- (pos-z  p) (*  (movekey-speed mk) dt (dir-dz d))))
      (dir-negate d))]
    [(equal? (movekey-key mk) "a")
     (move-with-collision
      p
      (pos
       (+ (pos-x  p) (*  (movekey-speed mk) dt (dir-dx yd)))
       (+ (pos-y  p) (*  (movekey-speed mk) dt (dir-dy yd)))
       (+ (pos-z  p) (*  (movekey-speed mk) dt (dir-dz yd))))
      yd)]
    [(equal? (movekey-key mk) "d")
     (move-with-collision
      p
      (pos
       (- (pos-x  p) (*  (movekey-speed mk) dt (dir-dx yd)))
       (- (pos-y  p) (*  (movekey-speed mk) dt (dir-dy yd)))
       (- (pos-z  p) (*  (movekey-speed mk) dt (dir-dz yd))))
      (dir-negate yd))]
    [(equal? (movekey-key mk) "shift")
     (move-with-collision
      p
      (pos
       (- (pos-x  p) (*  (movekey-speed mk) dt (dir-dx pd)))
       (- (pos-y  p) (*  (movekey-speed mk) dt (dir-dy pd)))
       (- (pos-z  p) (*  (movekey-speed mk) dt (dir-dz pd))))
      (dir-negate pd))]
    [(equal? (movekey-key mk) " ")
     (move-with-collision
      p
      (pos
       (+ (pos-x  p) (*  (movekey-speed mk) dt (dir-dx pd)))
       (+ (pos-y  p) (*  (movekey-speed mk) dt (dir-dy pd)))
       (+ (pos-z  p) (*  (movekey-speed mk) dt (dir-dz pd))))
      pd)]
    [else p]))

;;original position, moved position, a dir, time, and an orb-> moved position with any collisions accounted for
;;poc is pos of collision
(define (move-with-collision op mp d)
  (define poc (trace FINAL-LANDSCAPE op d))
  (cond
    [(equal? poc #f)
     mp]
    [(< (dir-dist (pos- op poc)) (dir-dist (pos- op mp)))
     (define back-dir (pos- op poc))
     (define dist (dir-dist back-dir))
     (cond
       [(zero? dist) poc]
       [else
        op
        #;
        (pos+ poc (dir-scale (dir-normalize back-dir) 1/2))])]
    [else mp]))

(module+ test (check-equal? (move-with-collision (pos 50 0 0) (pos 51 0 0) +x)
                            (pos 51 0 0)))