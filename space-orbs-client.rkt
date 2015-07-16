#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "frame-handling.rkt")

;; key and the speed it is moving in that direction, pos is the pos it had when it was pressed
(struct movekey (key speed) #:transparent)
;;position and time at last key change.list of moves. Pos is a list of 3 coordinates that is current. (continue on next line)
;;Dir is direction it is pointing and ang is how much it is rotated from normally. mx and my are mouse coordinates
(struct orb (pos time movekeys dir ang) #:transparent)
;;player and enemy are both orbs
(struct orbs (player enemy))

(define WALL-SIZE 70)
(define NUM-OF-CUBES 30)
(define NUM-OF-LIGHTS 20)
(define DEFAULTPOS (pos 15 -15 15))
(define DEFAULTPOS2 (pos WALL-SIZE (/ WALL-SIZE 2) (/ WALL-SIZE 2)))
(define DEFAULTDIR +y)
(define STARTING-SPEED 1/80)
(define ROTATION-SPEED-MULTIPLIER 4)
(define TESTORB (orb (pos 1 1 1) 5 '() (dir -1 0 0) 0))
(define DISCO? #f)

(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 1
                            #:roughness 0.2))

;################################################################################################################################################################################################ON-DRAW

;list of cubes and number of cubes to make-> list of cubes
(define (pick-random-cubes l n)
  (define random-cube
    (set-color
     (cube (pos (random (+ 1 (* WALL-SIZE 2))) (random (+ 1 WALL-SIZE)) (random (+ WALL-SIZE 1))) (+ 2 (random) (random)))
     (rgba "white")))
  (cond
    [(= n 0)
     l]
    [else
     (pick-random-cubes (cons random-cube l) (- n 1))]))

(define (pick-random-lights l n)
  (define random-light
    (light (pos (random (+ 1 (* WALL-SIZE 2))) (random (+ 1 WALL-SIZE)) (random (+ WALL-SIZE 1))) (emitted "white" 30)))
  (cond
    [(= n 0)
     l]
    [else
     (pick-random-lights (cons random-light l) (- n 1))]))

(define LIGHTS-LIST
  (pick-random-lights empty NUM-OF-LIGHTS))

(define LANDSCAPE-LIST
  (let ()
    (define W WALL-SIZE)
    (define H (* W 1/2))
    (append
     (pick-random-cubes empty NUM-OF-CUBES)
     (list
      (set-emitted
       (rectangle origin (pos 0 (- H) H))
       (emitted "blue" 2))
      (set-emitted
       (rectangle (pos H 0 0) (pos H (- H) H))
       (emitted "blue" 2))
      (set-emitted
       (rectangle origin (pos H (- H) 0))
       (emitted "blue" 2))
      (set-emitted
       (rectangle (pos 0 0 H) (pos H (- H) H))
       (emitted "blue" 2))
      (set-emitted
       (rectangle (pos 0 (- H) 0) (pos H (- H) H))
       (emitted "green" 2))
      (set-color
       (rectangle (pos H 0 0) (pos (* 2 W) 0 W))
       (rgba "lightgreen"))
      (set-color
       (rectangle (pos 0 0 H) (pos H 0 W))
       (rgba "lightgreen"))
      (set-color
       (rectangle origin (pos 0 W W))
       (rgba "red"))
      (set-color
       (rectangle (pos 0 W 0) (pos (* 2 W) W W))
       (rgba "blue"))
 #|     (set-color
       (rectangle origin (pos (* 2 W) W W))
       (rgba "yellow"))|#
      (set-color
       (rectangle (pos 0 0 W) (pos (* 2 W) W W))
       (rgba "purple"))
      (set-color
       (rectangle origin (pos (* 2 W) W 0))
       (rgba "orange"))))))

(define (on-draw os n t)
  (define p (orbs-player os))
  (define DRAW
    (combine
     (apply combine LANDSCAPE-LIST)
     (draw-enemy (orbs-enemy os) t)
     (lights+camera (current-pos p t) (orb-dir p) (current-ang p t))))
  (cond
    [(and (> t 0) (< t 2000))
     (maximize-screen)
     DRAW]
    [else DRAW]))

(define (draw-enemy o t)
  (sphere (current-pos o t) 1))

(define (lights+camera currentpos d ang)
  (combine (apply combine
                  (cond
                    [DISCO?
                     (pick-random-lights empty NUM-OF-LIGHTS)]
                    [else LIGHTS-LIST]))
           (basis 'camera (point-at
                           currentpos
                           d
                           #:angle ang))))

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
  (define pd (rotate-around-dir d (angles->dir yaw (+ pitch 90)) ang))
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
  (define poc (trace (apply combine LANDSCAPE-LIST) op d))
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

;;rotates to-rotate around around
;;takes two dir and an angle -> one dir
(define (rotate-around-dir around to-rotate ang)
  (rotate-around-coordinates
   (dir-dx to-rotate)
   (dir-dy to-rotate)
   (dir-dz to-rotate)
   (dir-dx around)
   (dir-dy around)
   (dir-dz around)
   ang))

;;rotates point x y z around origin and u v w
;;u v w has to be a unit vector
(define (rotate-around-coordinates x y z u v w deg-ang)
  (define ang (/ (* deg-ang pi) 180))
  (define cang (cos ang))
  (define sang (sin ang))
  (define (base g h)
    (+
     (*
      (- g)
      (- 0 (* u x) (* v y) (* w z))
      (- 1 cang))
     (* h cang)))
  (dir
   (+
    (base u x)
    (*
     (- (* v z) (* w y))
     sang))
   (+
    (base v y)
    (*
     (- (* w x) (* u z))
     sang))
   (+
    (base w z)
    (*
     (- (* u y) (* v x))
     sang))))

(module+ test (check-equal?
               (round-dir (rotate-around-coordinates 1 0 0 0 0 1 180))
               (dir -1 0 0)))
(module+ test (check-equal?
               (round-dir (rotate-around-coordinates 0 1 0 1 0 0 90))
               (dir 0 0 1)))

;;takes dir and angle -> dir
(define (rotation-to-dir dir ang)
  (define-values (yaw pitch) (dir->angles dir))
  (rotate-around-dir dir (angles->dir yaw (+ pitch 90)) ang))

(module+ test (rotation-to-dir +x 0) -z)

(define (dir-to-rotation basedir rotdir)
  (define downdir (rotation-to-dir basedir 0))
  (define leftdir (rotation-to-dir basedir 90))
  (define a
    (*
     (asin (min 1.0 (/ (dir-dist (dir- downdir rotdir)) 2)))
     2
     (/ 180 pi)))
  (cond
    [(< (dir-dist (dir- leftdir rotdir)) (sqrt 2))
     a]
    [(= (- 360 a) 360)
     0]
    [else
     (- 360 a)]))

(module+ test (check-equal? (round (dir-to-rotation -x +y)) 90.0))
(module+ test (check-equal? (round (dir-to-rotation -x -y)) 270.0))
(module+ test (check-equal? (round (dir-to-rotation -x (rotation-to-dir -x 50))) 50.0))
(module+ test (check-equal? (round (dir-to-rotation -x (rotation-to-dir -x 184))) 184.0))

(define (find-this-movekey key ms)
  (cond
    [(empty? ms)
     #f]
    [(equal? (movekey-key (first ms)) key)
     (first ms)]
    [else (find-this-movekey key (rest ms))]))

(module+ test (check-equal? (find-this-movekey "w" '())
                           #f))
(module+ test (check-equal? (find-this-movekey "a" (list (movekey "w" 1/2) (movekey "a" 1/2)))
                            (movekey "a" 1/2)))

(define (round-pos p)
  (pos
   (/ (round (* (pos-x p) 10000)) 10000)
   (/ (round (* (pos-y p) 10000)) 10000)
   (/ (round (* (pos-z p) 10000)) 10000)))

(define (round-dir d)
  (dir
   (/ (round (* (dir-dx d) 10000)) 10000)
   (/ (round (* (dir-dy d) 10000)) 10000)
   (/ (round (* (dir-dz d) 10000)) 10000)))

(define (round-orbs-dir o)
  (struct-copy orb o [dir (round-dir (orb-dir o))]))

;#################################################################################################################################################################################################ON-KEY

(define (on-key os n t key)
  (struct-copy orbs os
               [player (on-player-key (orbs-player os) n t key)]
               [enemy (orbs-enemy os)]))

(define (on-player-key o n t key)
  (define lkey (string-foldcase key))
  (cond
    [(find-this-movekey lkey (orb-movekeys o))
     o]
    [(or (equal? lkey "w") (equal? lkey "s") (equal? lkey "a") (equal? lkey "d") (equal? lkey " ") (equal? lkey "shift") (equal? lkey "q") (equal? lkey "e"))
     (struct-copy orb o
                  [pos (current-pos o t)]
                  [time t]
                  [movekeys (cons (movekey lkey STARTING-SPEED)  (orb-movekeys o))]
                  [ang (current-ang o t)])]
    [else o]))

(module+ test (check-equal? (on-player-key (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey " " 1))]) "n" 30 " ")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey " " 1))])))

(module+ test (check-equal? (on-player-key (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys empty]) "n" 5 "shift")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey "shift" STARTING-SPEED))])))

;#############################################################################################################################################################################################ON-RELEASE

(define (on-release os n t key)
    (struct-copy orbs os
               [player (on-player-release (orbs-player os) n t key)]
               [enemy (orbs-enemy os)]))

(define (on-player-release o n t key)
  (define lkey (string-foldcase key))
  (struct-copy orb
               o
               [pos (current-pos o t)]
               [time t]
               [movekeys (remove-this-movekey (orb-movekeys o) lkey)]
               [ang (current-ang o t)]))

(module+ test (check-equal? (on-player-release (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey " " 1))]) "n" 5 " ")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys empty])))

(define (remove-this-movekey ms key)
  (cond
    [(empty? ms) ms]
    [(equal? key (movekey-key (first ms)))
     (remove-this-movekey (rest ms) key)]
    [else
     (cons (first ms) (remove-this-movekey (rest ms) key))]))

;###############################################################################################################################################################################################ON-MOUSE

(define (on-mouse os n t x-ignored y-ignored e)
    (struct-copy orbs os
               [player (on-player-mouse (orbs-player os) n t e)]
               [enemy (orbs-enemy os)]))

(define (on-player-mouse o n t e)
  (define-values (x y) (get-mouse-delta))
  (define n (new-dir-and-ang o x y t))
  (struct-copy
   orb
   o
   [pos (current-pos o t)]
   [time t]
   [dir (first n)]
   [ang (second n)]))

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

;################################################################################################################################################################################################BIGBANG

(big-bang3d (orbs (orb DEFAULTPOS 0 '() DEFAULTDIR 0) (orb DEFAULTPOS2 0 '() DEFAULTDIR 0))
            #:name "Space Orbs"
            #:on-draw on-draw
            #:on-key on-key
            #:on-release on-release
            #:on-mouse on-mouse)