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
  (define s (movekey-speed mk))
  (cond
    [(equal? (movekey-key mk) "w")
     (move-with-collision
      p
      (pos+ p (dir-scale d (* dt s)))
      d
      dt)]
    [(equal? (movekey-key mk) "s")
     (move-with-collision
      p
      (pos+ p (dir-scale (dir-negate d) (* dt s)))
      (dir-negate d)
      dt)]
    [(equal? (movekey-key mk) "a")
     (move-with-collision
      p
      (pos+ p (dir-scale yd (* dt s)))
      yd
      dt)]
    [(equal? (movekey-key mk) "d")
     (move-with-collision
      p
      (pos+ p (dir-scale (dir-negate yd) (* dt s)))
      (dir-negate yd)
      dt)]
    [(equal? (movekey-key mk) "shift")
     (move-with-collision
      p
      (pos+ p (dir-scale (dir-negate pd) (* dt s)))
      (dir-negate pd)
      dt)]
    [(equal? (movekey-key mk) " ")
     (move-with-collision
      p
      (pos+ p (dir-scale pd (* dt s)))
      pd
      dt)]
    [else p]))

;;original position, moved position, a dir, and delta time-> moved position with any collisions accounted for
;;poc is pos of collision and sd is surface dir, the dir perpendicular to the surface
(define (move-with-collision op mp d dt)
  (define-values (poc sd) (trace/normal FINAL-LANDSCAPE op d))
  (cond
    [(equal? poc #f)
     mp]
    [(< (dir-dist (pos- op poc)) (dir-dist (pos- op mp)));if poc is closer (if there is a collision)
     (try-to-slide op poc d sd dt)]
    [else mp]))

;sp is a slided pos and posc is point of slide collision
(define (try-to-slide op poc d sd dt)
  (define sp (slide-against-surface op poc d sd dt))
  (define slide-dir (rotate-around-dir (dir-cross d sd) sd 90))
  (define posc (trace FINAL-LANDSCAPE op slide-dir))
  (cond
    [(equal? posc #f)
     sp]
    [(< (dir-dist (pos- op posc)) (dir-dist (pos- op sp)))
     op]
    [else sp]));;FIXME- bumpy when holding against a corner and going foward

;old position, point of collision, dir, and surface dir-> new pos
(define (slide-against-surface op poc d sd dt)
  (define slide-dir (rotate-around-dir (dir-cross d sd) sd 90))
  (define slide-speed (* SLIDE-SPEED-MULTIPLIER (dir-to-rotation d sd)))
  (pos
   (+ (pos-x  op) (*  slide-speed dt (dir-dx slide-dir)))
   (+ (pos-y  op) (*  slide-speed dt (dir-dy slide-dir)))
   (+ (pos-z  op) (*  slide-speed dt (dir-dz slide-dir)))))

;note- write tests for move-with-collision!
