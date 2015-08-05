#lang racket
(require pict3d "variables.rkt" "structures.rkt")
(provide FINAL-LANDSCAPE pick-random-lights LIGHTS-LIST TEST-LAND set-cubes)

(define TEST-LAND
  (freeze
   (combine
    (rectangle (pos 0 10 0) (pos 10 20 10))
    (rectangle (pos 10 0 0) (pos 20 10 10)))))

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

;;list of mycubes-> list of cubes
(define (make-cube-list l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons
      (set-color
       (cube (mycube-pos (first l)) (mycube-scale (first l)))
       (rgba (mycube-color (first l))))
      (make-cube-list (rest l)))]))

(define (make-landscape mycube-list)
  (let ()
    (define W WALL-SIZE)
    (define H (* W 1/2))
    (define S (* H 1/2))
    (define T (* S 1/2))
    (append
     (make-cube-list mycube-list)
     (list
      (set-emitted
       (rectangle (pos -0.1 0 0) (pos 0 (- H) H))
       (emitted "blue" 2));from spawn, left blue square
     (set-emitted
       (rectangle (pos (+ H 0.1) 0 0) (pos H (- H) H))
       (emitted "blue" 2));right blue square
      (set-emitted
       (rectangle (pos 0 0 -0.1) (pos H (- H) 0))
       (emitted "blue" 2));bottom blue square
      (set-emitted
       (rectangle (pos 0 0 (+ H 0.1)) (pos H (- H) H))
       (emitted "blue" 2));top blue square
      (set-emitted
       (rectangle (pos 0 (- (- H) 0.1) 0) (pos H (- H) H))
       (emitted "green" 2));back green square
      (set-color
       (rectangle (pos H -0.1 0) (pos (* 2 W) 0 W))
       (rgba "lightgreen"));back green wall
      (set-color
       (rectangle (pos 0 -0.1 H) (pos H 0 W))
       (rgba "lightgreen"));extention of back green wall above spawn
      (set-color
       (rectangle origin (pos -0.1 (+ T S) W))
       (rgba "white"));close part of left red wall
      (set-color
       (rectangle (pos 0 (+ T S) 0) (pos -0.1 (+ H T) (+ S T)))
       (rgba "white"));bottom small part of left red wall
      (set-color
       (rectangle (pos 0 (+ T S) (+ H T)) (pos -0.1 (+ H T) W))
       (rgba "white"));top small part of left red wall
      (set-color
       (rectangle (pos 0 (+ T H) 0) (pos -0.1 W W))
       (rgba "white"));far part of left red wall
      (set-color
       (rectangle (pos 0 (+ W 0.1) 0) (pos (* 2 W) W W))
       (rgba "yellow"));front blue wall
      (set-color
       (rectangle (pos (+ (* 2 W) 0.1) 0 0) (pos (* 2 W) W W))
       (rgba "white"));right yellow wall
      (set-color
       (rectangle (pos 0 0 (+ 0.1 W)) (pos (* 2 W) W W))
       (rgba "purple"));top purple wall
      (set-color
       (rectangle origin (pos (* 2 W) W -0.1))
       (rgba "orange"))))));bottom orange wall

(define FINAL-LANDSCAPE
  (freeze
   (apply combine (make-landscape empty))))

(define (set-cubes l)
  (set!
   FINAL-LANDSCAPE
   (freeze
    (apply combine (make-landscape l)))))