#lang racket
(require pict3d rackunit pict3d/universe "structures.rkt")
(provide (all-defined-out))

(define WALL-SIZE 80)
(define NUM-OF-CUBES 30)
(define NUM-OF-LIGHTS 30)

(define DEFAULTPOS (pos 15 -15 15))
(define DEFAULTPOS2 (pos (- (* WALL-SIZE 2) 15) (+ WALL-SIZE 15) (- WALL-SIZE 15)))
(define DEFAULTDIR +y)
(define DEFAULTDIR2 -y)
(define STARTING-SPEED 1/100)
(define SLIDE-SPEED 1/150)
(define ROTATION-SPEED-MULTIPLIER 4)
(define DEFAULT-STATE
  (game
   'deathmatch
   (orbs (orb DEFAULTPOS 0 empty-movekeys DEFAULTDIR 0 empty 0 "1" "blue" #f #f 0 0) empty)
   #f
   #f
   0
   (set)))
(define DEFAULT-ORB (orb DEFAULTPOS 0 empty-movekeys DEFAULTDIR 0 empty 0 "1" "blue" #f #f 0 0))

(define ORB-RADIUS 2)

(define RELOAD-SPEED 1000);in milliseconds
(define SHOT-LIFE 700);in milliseconds
(define SHOT-WIDTH 0.08);radius of shot

(define UPDATE-SPEED 100);speed it sends state and updates the key times and old positions in milliseconds
(define MASTER-TIME-OFFSET 0);;set! variable for how much to offset the time, to keep time consistant for new connects- I think it works ok
(define PORT 50002)
(define SERVER-ADRESS "10.0.1.12")
(define CLIENT-ADRESS "c-67-166-78-233.hsd1.ut.comcast.net")

(define TESTORB (orb (pos 1 1 1) 5 empty-movekeys (dir -1 0 0) 0 empty 0 "Bob" "blue" #f #f 0 0))

(define DISCO? #f)

(define USE-MOUSE-CURVE? false)
(define MOUSE-SENSITIVITY 1/4);;higher is more "sensitive" which means less movement means more in game movement
(define MOUSE-MOVE-WAVE-MAX 200);;anything under 200 is applied to the cosng wave for sensitivity

(define (set-offset t)
  (println "s")
  (set! MASTER-TIME-OFFSET (+ MASTER-TIME-OFFSET t)))

;;cache for drawing the pict for the number of deaths in the scoreboard. set! variable
;;it is a list of the number of the pict and the pict
(define DEATHS-NUM-CACHE
  (list 80 empty-pict3d))

(define KILLS-NUM-CACHE
  (list 80 empty-pict3d))

(define (set!KILLS-NUM-CACHE l)
  (set! KILLS-NUM-CACHE l))

(define (set!DEATHS-NUM-CACHE l)
  (set! DEATHS-NUM-CACHE l))