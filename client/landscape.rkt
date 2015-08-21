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

(define (make-landscape mycube-list);Carson Hahn works on this
  (let ()
    (define W WALL-SIZE)
    (define D (* W 2))
    (define H (* W 1/2))
    (define WH (+ W H))
    (define S (* H 1/2))
    (define T (* S 1/2))
    (append
     (make-cube-list mycube-list)
     (list
      (set-emitted
       (rectangle (pos -0.1 0 0) (pos 0 (- H) H))
       (emitted "green" 2));from spawn, left green square spawn
     (set-emitted
       (rectangle (pos (+ H 0.1) 0 0) (pos H (- H) H))
       (emitted "green" 2));right green square spawn
      (set-emitted
       (rectangle (pos 0 0 -0.1) (pos H (- H) 0))
       (emitted "green" 2));bottom green square spawn
      (set-emitted
       (rectangle (pos 0 0 (+ H 0.1)) (pos H (- H) H))
       (emitted "green" 2));top green square spawn
      (set-emitted
       (rectangle (pos 0 (- (- H) 0.1) 0) (pos H (- H) H))
       (emitted "blue" 2));back blue square blue spawn
      (set-emitted
       (rectangle (pos 0 (+ S T) (+ H T(- 0.1))) (pos (- 0.2) (+ H T) (+ H T)))
       (emitted "blue" 2));top border blue hole
      (set-emitted
       (rectangle (pos 0 (+ S T) (+ S T)) (pos (- 0.1) (+ S T 0.2) (+ H T)))
       (emitted "blue" 2));left border blue hole
      (set-emitted
       (rectangle (pos 0 (+ H T) (+ S T)) (pos (- 0.1) (+ H T (- 0.2)) (+ H T)))
       (emitted "blue" 2));right border blue hole
      (set-emitted
       (rectangle (pos 0 (+ S T) (+ S T)) (pos (- 0.1) (+ H T) (+ S T 0.2)))
       (emitted "blue" 2));bottom border blue hole
      (set-emitted
       (rectangle (pos D (+ S T) (+ H T)) (pos (+ D (- 0.1)) (+ H T) (+ H T (- 0.2))))
       (emitted "red" 2));top border red hole
      (set-emitted
       (rectangle (pos D (+ S T) (+ H T)) (pos (+ D (- 0.1)) (+ S T 0.2) (+ S T)))
       (emitted "red" 2));right border red hole
      (set-emitted
       (rectangle (pos D (+ H T) (+ H T)) (pos (+ D (- 0.1)) (+ H T (- 0.2)) (+ S T)))
       (emitted "red" 2));left border red hole
      (set-emitted
       (rectangle (pos D (+ S T) (+ S T)) (pos (+ D (- 0.1)) (+ H T) (+ S T 0.2)))
       (emitted "red" 2));bottom border red hole
      (set-emitted
       (rectangle (pos (- WH 0.1) W H) (pos WH WH W))
       (emitted "green" 2));right wall from red spawn
      (set-emitted
       (rectangle (pos (- D 0.1) W H) (pos D WH W))
       (emitted "green" 2));left wall from red spawn
      (set-emitted
       (rectangle (pos WH W (- H 0.1)) (pos D WH H))
       (emitted "green" 2));bottom wall from red spawn
      (set-emitted
       (rectangle (pos WH W (+ W -0.1)) (pos D WH W))
       (emitted "green" 2));top square of red spawn
      (set-emitted
       (rectangle (pos WH WH H) (pos D (+ WH 0.1) W))
       (emitted "red" 2));back red wall of red spawn
      (set-color
       (rectangle (pos H -0.1 0) (pos (* 2 W) 0 W))
       (rgba "lightgreen"));back green wall
      (set-color
       (rectangle (pos 0 -0.1 H) (pos H 0 W))
       (rgba "lightgreen"));extention of back green wall above spawn
      (set-color
       (rectangle origin (pos -0.1 (+ T S) W))
       (rgba "white"));close part of left white wall
      (set-color
       (rectangle (pos 0 (+ T S) 0) (pos -0.1 (+ H T) (+ S T)))
       (rgba "white"));bottom small part of left white wall
      (set-color
       (rectangle (pos 0 (+ T S) (+ H T)) (pos -0.1 (+ H T) W))
       (rgba "white"));top small part of left white wall
      (set-color
       (rectangle (pos 0 (+ T H) 0) (pos -0.1 W W))
       (rgba "white"));far part of left white wall
      (set-color
       (rectangle (pos 0 (+ W 0.1) 0) (pos (- D H) W W))
       (rgba "yellow"));front yellow wall
      (set-color
       (rectangle (pos (+ W H) (+ W 0.1) 0) (pos D W H))
       (rgba "yellow"));extension of front yellow wall
      (set-color
       (rectangle (pos 0 0 (+ 0.1 W)) (pos (* 2 W) W W))
       (rgba "purple"));top purple wall
      (set-color
       (rectangle origin (pos (* 2 W) W -0.1))
       (rgba "orange"));bottom orange wall
      (set-color
       (rectangle (pos D 0 0) (pos (+ D -0.1) (+ T S) W))
       (rgba "white"));close part of right white wall
      (set-color
       (rectangle (pos D (+ T S) 0) (pos (+ D -0.1) (+ H T) (+ S T)))
       (rgba "white"));bottom small part of right white wall
      (set-color
       (rectangle (pos D (+ T S) (+ H T)) (pos (+ D -0.1) (+ H T) W))
       (rgba "white"));top small part of right white wall
      (set-color
       (rectangle (pos D (+ T H) 0) (pos (+ D -0.1) W W))
       (rgba "white"))))));far part of right white wall

(define FINAL-LANDSCAPE
  (freeze
   (apply combine (make-landscape empty))))

(define (set-cubes l)
  (set!
   FINAL-LANDSCAPE
   (freeze
    (apply combine (make-landscape l)))))