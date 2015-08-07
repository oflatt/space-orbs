#lang racket
(require pict3d)
(provide (struct-out movekey) (struct-out orb) (struct-out orbs) (struct-out game) (struct-out shot) (struct-out client) (struct-out message) (struct-out orbdefine)
         (struct-out mypos) (struct-out mydir) (struct-out mycube) round-pos round-dir round-orbs-dir)

;; key and the speed it is moving in that direction, pos is the pos it had when it was pressed
(struct movekey (key speed) #:prefab)
;corner 1 and 2 are pos for drawing and pos is where to move it
;time is time in milliseconds when it was shot
(struct shot (corner1 corner2 pos yaw pitch time) #:prefab)
;;position and time at last key change.list of moves. Pos is a list of 3 coordinates that is current. (continue on next line)
;;Dir is direction it is pointing and roll is how much the camera is rotated. mx and my are mouse coordinates
;;shots is a list of shots to draw and reload-time is time in milliseconds before the player may shoot again
;;name and color are strings
(struct orb (pos time movekeys dir roll shots reload-time name color hostname port) #:prefab)
;;player is a orb and enemys is a list of orbs
(struct orbs (player enemys) #:transparent)
;orbs is an orbs and exit? is wheather or not to stop the state and close the window
;;mt is the time in milliseconds at last send of state
(struct game (orbs exit? mt) #:transparent)


(struct client (hostname port) #:prefab)
(struct message (name data) #:prefab)
(struct orbdefine (name color hostname port) #:prefab)

(struct mypos (x y z) #:prefab)
(struct mydir (dx dy dz) #:prefab)
(struct mycube (pos scale color) #:prefab)

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