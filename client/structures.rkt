#lang racket
(require pict3d lens unstable/lens)
(provide (all-defined-out))

;; speeds in the given key directions
;; the keys s, d, shift, and q are represented by negetive numbers
;;   in the w, a, space, and e fields.
(struct/lens movekeys (w a space e) #:prefab)
;corner 1 and 2 are pos for drawing and pos is where to move it
;time is time in milliseconds when it was shot
(struct/lens shot (corner1 corner2 pos yaw pitch time) #:prefab)
;;position and time at last key change.list of moves. Pos is a list of 3 coordinates that is current. (continue on next line)
;;Dir is direction it is pointing and roll is how much the camera is rotated. mx and my are mouse coordinates
;;shots is a list of shots to draw and reload-time is time the player shot last
;;name and color are strings
(struct/lens orb (pos time movekeys dir roll shots reload-time name color hostname port kills deaths) #:prefab)
;;player is a orb and enemys is a list of orbs
(struct/lens orbs (player enemys) #:transparent);;enemys includes teammates, confusing!
;orbs is an orbs and exit? is wheather or not to stop the state and close the window
;;scores? is whether or not tab is pressed, to show scores ect.
;;mt is the time in milliseconds at last update and send of state
;;held-keys is a set containing the keys that are currently pressed down, and
;;  that we don't want to react to until they are released
(struct/lens game (mode orbs exit? scores? mt held-keys) #:transparent)

(define-nested-lenses [game-orbs game-orbs-lens]
  [enemys orbs-enemys-lens]
  [player orbs-player-lens
    [pos orb-pos-lens]
    [time orb-time-lens]
    [shots orb-shots-lens]
    [deaths orb-deaths-lens]
    [kills orb-kills-lens]])

(struct/lens client (hostname port last-message-time) #:prefab);;used by server to keep track of clients
(struct/lens message (name data) #:prefab);;this is what is sent between clients and server
(struct/lens orbdefine (name color hostname port) #:prefab);;used for sending info about an orb for kills, new orbs, ect.

(struct/lens mypos (x y z) #:prefab)
(struct/lens mydir (dx dy dz) #:prefab)
(struct/lens mycube (pos scale color) #:prefab)

(define empty-movekeys
  (movekeys 0 0 0 0))

(define (movekeys+w mks w2)
  (lens-transform movekeys-w-lens mks (λ (w) (+ w w2))))
(define (movekeys+a mks a2)
  (lens-transform movekeys-a-lens mks (λ (a) (+ a a2))))
(define (movekeys+space mks space2)
  (lens-transform movekeys-space-lens mks (λ (space) (+ space space2))))
(define (movekeys+e mks e2)
  (lens-transform movekeys-e-lens mks (λ (e) (+ e e2))))

(define (movekeys+s mks s)
  (movekeys+w mks (- s)))
(define (movekeys+d mks d)
  (movekeys+a mks (- d)))
(define (movekeys+shift mks shift)
  (movekeys+space mks (- shift)))
(define (movekeys+q mks q)
  (movekeys+e mks (- q)))

(define (w-movekey w)
  (movekeys w 0 0 0))
(define (a-movekey a)
  (movekeys 0 a 0 0))
(define (space-movekey space)
  (movekeys 0 0 space 0))
(define (e-movekey e)
  (movekeys 0 0 0 e))

(define (s-movekey s)
  (w-movekey (- s)))
(define (d-movekey d)
  (a-movekey (- d)))
(define (shift-movekey shift)
  (space-movekey (- shift)))
(define (q-movekey q)
  (e-movekey (- q)))

(define/match (pos->mypos p)
  [[(pos x y z)]
   (mypos x y z)])
(define/match (mypos->pos p)
  [[(mypos x y z)]
   (pos x y z)])

(define/match (dir->mydir d)
  [[(dir x y z)]
   (mydir x y z)])
(define/match (mydir->dir d)
  [[(mydir x y z)]
   (dir x y z)])

(define (round-10000th x)
  (/ (round (* x 10000)) 10000))

(define/match (round-pos p)
  [[(pos x y z)]
   (pos (round-10000th x)
        (round-10000th y)
        (round-10000th z))])

(define/match (round-dir d)
  [[(dir dx dy dz)]
   (dir (round-10000th dx)
        (round-10000th dy)
        (round-10000th dz))])

(define (round-orbs-dir o)
  (struct-copy orb o [dir (round-dir (orb-dir o))]))

(define/match (pos->dir p)
  [[(pos x y z)]
   (dir x y z)])
