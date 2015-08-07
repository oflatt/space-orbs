#lang racket
(require pict3d rackunit pict3d/universe "structures.rkt")
(provide (all-defined-out))

(define WALL-SIZE 80)
(define NUM-OF-CUBES 30)
(define NUM-OF-LIGHTS 30)

(define DEFAULTPOS (pos 15 -15 15))
(define DEFAULTPOS2 (pos WALL-SIZE (/ WALL-SIZE 2) (/ WALL-SIZE 2)))
(define DEFAULTDIR +y)
(define STARTING-SPEED 1/100)
(define SLIDE-SPEED 1/150)
(define ROTATION-SPEED-MULTIPLIER 4)
(define DEFAULT-STATE
  (game
   (orbs (orb DEFAULTPOS 0 '() DEFAULTDIR 0 empty 0 "1" "blue" #f #f) empty)
   #f
   0))
(define DEFAULT-ORB (orb DEFAULTPOS 0 '() DEFAULTDIR 0 empty 0 "1" "blue" #f #f))

(define ORB-RADIUS 2)

(define SHOT-LIFE 700);in milliseconds
(define SHOT-WIDTH 1);radius of shot

(define SEND-SPEED 400);speed it sends state in milliseconds
(define MASTER-TIME-OFFSET 0)
(define PORT 50002)
(define SERVER-ADRESS "10.0.1.12")
(define CLIENT-ADRESS "c-67-166-78-233.hsd1.ut.comcast.net")

(define TESTORB (orb (pos 1 1 1) 5 '() (dir -1 0 0) 0 empty 0 "Bob" "blue" #f #f))

(define DISCO? #f)

(define (set-offset t)
  (println "s")
  (set! MASTER-TIME-OFFSET (+ MASTER-TIME-OFFSET t)))