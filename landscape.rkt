#lang racket
(require pict3d "variables.rkt")
(provide FINAL-LANDSCAPE pick-random-lights LIGHTS-LIST)

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
       (rectangle origin (pos 0 (- H) H))
       (emitted "blue" 2))
      (set-emitted
       (rectangle (pos H 0 0) (pos H (- H) H))
       (emitted "blue" 2))
      (set-emitted
       (rectangle origin (pos H (- H) 0))
       (emitted "blue" 2))
      (set-emitted
       (rectangle (pos 0 0 H) (pos H (- H) H))
       (emitted "blue" 2))
      (set-emitted
       (rectangle (pos 0 (- H) 0) (pos H (- H) H))
       (emitted "green" 2))
      (set-color
       (rectangle (pos H 0 0) (pos (* 2 W) 0 W))
       (rgba "lightgreen"))
      (set-color
       (rectangle (pos 0 0 H) (pos H 0 W))
       (rgba "lightgreen"))
      (set-color
       (rectangle origin (pos 0 W W))
       (rgba "red"))
      (set-color
       (rectangle (pos 0 W 0) (pos (* 2 W) W W))
       (rgba "blue"))
 #|   (set-color
       (rectangle origin (pos (* 2 W) W W))
       (rgba "yellow"))| FIXME: weird wall bug|#
      (set-color
       (rectangle (pos 0 0 W) (pos (* 2 W) W W))
       (rgba "purple"))
      (set-color
       (rectangle origin (pos (* 2 W) W 0))
       (rgba "orange"))))))

(define FINAL-LANDSCAPE
  (freeze
   (apply combine LANDSCAPE-LIST)))