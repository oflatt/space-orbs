#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "structures.rkt")
(provide WALL-SIZE NUM-OF-CUBES NUM-OF-LIGHTS DEFAULTPOS DEFAULTPOS2 DEFAULTDIR STARTING-SPEED SLIDE-SPEED-MULTIPLIER ROTATION-SPEED-MULTIPLIER TESTORB DISCO?)

(define WALL-SIZE 70)
(define NUM-OF-CUBES 30)
(define NUM-OF-LIGHTS 20)

(define DEFAULTPOS (pos 15 -15 15))
(define DEFAULTPOS2 (pos WALL-SIZE (/ WALL-SIZE 2) (/ WALL-SIZE 2)))
(define DEFAULTDIR +y)
(define STARTING-SPEED 1/100)
(define SLIDE-SPEED-MULTIPLIER 0.0001)
(define ROTATION-SPEED-MULTIPLIER 4)

(define TESTORB (orb (pos 1 1 1) 5 '() (dir -1 0 0) 0 empty 0))

(define DISCO? #f)