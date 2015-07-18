#lang racket
(require pict3d)
(provide (struct-out movekey) (struct-out orb) (struct-out orbs) (struct-out game) round-pos round-dir round-orbs-dir)

;; key and the speed it is moving in that direction, pos is the pos it had when it was pressed
(struct movekey (key speed) #:transparent)
;;position and time at last key change.list of moves. Pos is a list of 3 coordinates that is current. (continue on next line)
;;Dir is direction it is pointing and ang is how much it is rotated from normally. mx and my are mouse coordinates
;;shots is a list of Pict3Ds to draw and reload-time is time in milliseconds before the player may shoot again
(struct orb (pos time movekeys dir ang shots reload-time) #:transparent)
;;player and enemy are both orbs
(struct orbs (player enemy) #:transparent)
;orbs is an orbs and exit? is wheather or not to stop the state and close the window
(struct game (orbs exit?))

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