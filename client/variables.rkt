#lang racket
(require pict3d rackunit pict3d/universe "structures.rkt")
(provide WALL-SIZE NUM-OF-CUBES NUM-OF-LIGHTS DEFAULTPOS DEFAULTPOS2 DEFAULTDIR DEFAULT-STATE STARTING-SPEED SLIDE-SPEED ROTATION-SPEED-MULTIPLIER SHOT-LIFE SHOT-WIDTH TESTORB DISCO? SEND-SPEED MASTER-TIME-OFFSET set-offset)

(define WALL-SIZE 70)
(define NUM-OF-CUBES 30)
(define NUM-OF-LIGHTS 20)

(define DEFAULTPOS (pos 15 -15 15))
(define DEFAULTPOS2 (pos WALL-SIZE (/ WALL-SIZE 2) (/ WALL-SIZE 2)))
(define DEFAULTDIR +y)
(define STARTING-SPEED 1/100)
(define SLIDE-SPEED 1/150)
(define ROTATION-SPEED-MULTIPLIER 4)
(define DEFAULT-STATE
  (game
   (orbs (orb DEFAULTPOS 0 '() DEFAULTDIR 0 empty 0 "Bob" "blue") (orb DEFAULTPOS2 0 '() DEFAULTDIR 0 empty 0 "Pie" "red"))
   #f
   0))

(define SHOT-LIFE 700);in milliseconds
(define SHOT-WIDTH 1);radius of shot

(define SEND-SPEED 100);speed it sends state in milliseconds
(define MASTER-TIME-OFFSET 0)

(define TESTORB (orb (pos 1 1 1) 5 '() (dir -1 0 0) 0 empty 0 "Bob" "blue"))

(define DISCO? #f)

(define (set-offset t)
  (println t)
  (set! MASTER-TIME-OFFSET t))