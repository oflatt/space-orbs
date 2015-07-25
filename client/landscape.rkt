#lang racket
(require pict3d "variables.rkt")
(provide FINAL-LANDSCAPE pick-random-lights LIGHTS-LIST TEST-LAND)

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

(define LANDSCAPE-LIST
  (let ()
    (define W WALL-SIZE)
    (define H (* W 1/2))
    (append
     (pick-random-cubes empty NUM-OF-CUBES)
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
       (rectangle origin (pos -0.1 W W))
       (rgba "red"));left red wall
      (set-color
       (rectangle (pos 0 (+ W 0.1) 0) (pos (* 2 W) W W))
       (rgba "blue"));front blue wall
      (set-color
       (rectangle (pos (+ (* 2 W) 0.1) 0 0) (pos (* 2 W) W W))
       (rgba "yellow"));right yellow wall
      (set-color
       (rectangle (pos 0 0 (+ 0.1 W)) (pos (* 2 W) W W))
       (rgba "purple"));top purple wall
      (set-color
       (rectangle origin (pos (* 2 W) W -0.1))
       (rgba "orange"))))));bottom orange wall

(define FINAL-LANDSCAPE
  (freeze
   (apply combine LANDSCAPE-LIST)))