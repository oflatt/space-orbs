#lang racket
(require pict3d lens unstable/lens rackunit)
(provide (all-defined-out))

;;FOR CLIENT----------------------------------------------------------------------------------------------------------------

;; speeds in the given key directions
;; the keys s, d, shift, and q are represented by negetive numbers
;;   in the w, a, space, and e fields.
(struct/lens movekeys (w a space e) #:prefab)
;corner 1 and 2 are pos for drawing and pos is where to move it
;time is time in milliseconds when it was shot
(struct/lens shot (corner1 corner2 pos yaw pitch time) #:prefab)
;;position and time at last key change.list of moves. Pos is a list of 3 coordinates that is current.
;;Dir is direction it is pointing and roll is how much the camera is rotated. mx and my are mouse coordinates
;;shots is a list of shots to draw and reload-time is time the player shot last
;;name and color are strings
(struct/lens orb (pos time movekeys dir roll shots reload-time name color hostname port kills deaths) #:prefab)
;;number defines which team it is
;;members is a list of orbs- excluding the player if it is the player-team
;;red, blue, and green are numbers for the color of the team
(struct/lens team (number members red green blue) #:transparent)
;;player is a orb, player-team is the team with all the player's teammates, and enemy-teams is a list of teams
;;exit? is wheather or not to stop the state and close the window
;;scores? is whether or not tab is pressed, to show scores ect.
;;mt is the time in milliseconds at last update and send of state
;;held-keys is a set containing the keys that are currently pressed down, and
;;  that we don't want to react to until they are released
;;not-connected? is a number for the last time it tried to connect to the server, or false if it is connected
(struct/lens game (mode player player-team enemy-teams exit? scores? mt held-keys not-connected?) #:transparent)

(define-nested-lenses [game-player game-player-lens]
  [pos orb-pos-lens]
  [time orb-time-lens]
  [shots orb-shots-lens]
  [deaths orb-deaths-lens]
  [kills orb-kills-lens])

(define (update-mt g t)
  (lens-transform
   game-mt-lens
   g
   (lambda (mt)
     t)))

;;takes a game and a function -> game
;;the function takes a list of orbs from the first enemy team in the enemy-teams list
;;for now serves the purpose of simplyfying the structures when changing a list of enemys
;;to get space orbs working with the new structures before changing it
(define (simple-change-enemys g function)
  (lens-transform
   game-enemy-teams-lens
   g
   (lambda (enemy-teams)
     (cons
      (lens-transform
       team-members-lens
       (first enemy-teams);;the first enemy team
       (lambda (members)
         (function members)));;call the function on the list of orbs
      (rest enemy-teams)))))

(module+ test
  (check-equal?
   (simple-change-enemys
    (game
     'deathmatch
     (orb (pos 1 0 0) 0 empty-movekeys (dir 1 0 0) 0 empty 0 "1" "blue" #f #f 0 0)
     (team 1 empty 0 0 0)
     (list (team 2 empty 0 0 0))
     #f
     #f
     0
     (set))
    (lambda (enemys)
      (orb (pos 1 0 0) 0 empty-movekeys (dir 1 0 0) 0 empty 0 "1" "red" #f #f 0 0)))
   (game
     'deathmatch
     (orb (pos 1 0 0) 0 empty-movekeys (dir 1 0 0) 0 empty 0 "1" "blue" #f #f 0 0)
     (team 1 empty 0 0 0)
     (list (team 2 (orb (pos 1 0 0) 0 empty-movekeys (dir 1 0 0) 0 empty 0 "1" "red" #f #f 0 0) 0 0 0))
     #f
     #f
     0
     (set))))

;;FOR MAPS-----------------------------------------------------------------------------------------------------------

;;positions is a list of pos
;;directions is a list of dir
;;positions and directions are parallel, ordered lists
;;red, green, and blue are numbers
(struct/lens spawn (positions directions red green blue))
;;name is a symbol and spawns is a list of spawn
;;a space-map would not travel in a message, it would be named in gamemode-info
(struct/lens space-map (name spawns frozen-pict) #:transparent)

;;FOR SERVER/MESSAGES---------------------------------------------------------------------------------------------

(struct/lens client (hostname port last-message-time) #:prefab);;used by server to keep track of clients
;;name is a string and client is a client for that orb. Kills, deaths, and team are numbers
(struct/lens server-orb (name client kills deaths team))
;;a game from the server's perspective
;;gamemode and map are symbols
;;orbs is a list of server-orb
;;start-time is a the current-milliseconds that the game was started
(struct/lens server-game (gamemode map orbs start-time))

(struct/lens message (name data) #:prefab);;this is what is sent between clients and server
(struct/lens orbdefine (name color hostname port) #:prefab);;used for sending info about an orb for kills, new orbs, ect.

;;gamemode and map are both symbols
;;this would be passed as a message to the client
(struct/lens gamemode-info (gamemode map))

;;name is a symbol and data is a structure for mycubes or text for sending a message from the server
(struct/lens server-landscape (name data) #:prefab)

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
