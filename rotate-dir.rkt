#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "structures.rkt")
(provide rotate-around-dir rotation-to-dir dir-to-rotation rotate-up rotate-down rotate-left rotate-right)

;;rotates to-rotate around around
;;It rotates to-rotate couterclockwise looking in the direction of around
;;takes two dir and an angle -> one dir
(define (rotate-around-dir to-rotate around ang)
  (define normal-around (dir-normalize around))
  (rotate-around-coordinates
   (dir-dx to-rotate)
   (dir-dy to-rotate)
   (dir-dz to-rotate)
   (dir-dx normal-around)
   (dir-dy normal-around)
   (dir-dz normal-around)
   ang))

;;rotates point x y z around origin and u v w
;;u v w has to be a unit vector
(define (rotate-around-coordinates x y z u v w deg-ang)
  (define ang (/ (* deg-ang pi) 180))
  (define cang (cos ang))
  (define sang (sin ang))
  (define (base g h)
    (+
     (*
      (- g)
      (- 0 (* u x) (* v y) (* w z))
      (- 1 cang))
     (* h cang)))
  (dir
   (+
    (base u x)
    (*
     (- (* v z) (* w y))
     sang))
   (+
    (base v y)
    (*
     (- (* w x) (* u z))
     sang))
   (+
    (base w z)
    (*
     (- (* u y) (* v x))
     sang))))

(module+ test (check-equal?
               (round-dir (rotate-around-coordinates 1 0 0 0 0 1 180))
               (dir -1 0 0)))
(module+ test (check-equal?
               (round-dir (rotate-around-coordinates 0 1 0 1 0 0 90))
               (dir 0 0 1)))

;;takes dir and angle -> dir
(define (rotation-to-dir d ang)
  (rotate-around-dir (rotate-up d) d ang))

(module+ test (rotation-to-dir +x 0) -z)

;gives angle between two dir
(define (dir-to-rotation basedir rotdir)
  (define downdir (rotation-to-dir basedir 0))
  (define leftdir (rotation-to-dir basedir 90))
  (define a
    (*
     (asin (min 1.0 (/ (dir-dist (dir- downdir rotdir)) 2)))
     2
     (/ 180 pi)))
  (cond
    [(< (dir-dist (dir- leftdir rotdir)) (sqrt 2))
     a]
    [(= (- 360 a) 360)
     0]
    [else
     (- 360 a)]))

(define (rotate-up d [ang 90])
  (define-values (yaw pitch) (dir->angles d))
  (angles->dir yaw (+ pitch ang)))

(module+ test
  (check-equal?
   (round-dir (rotate-up +x))
   +z))
(module+ test
  (round-dir (rotate-up -y 45))
  (dir 0.0 -0.7071 0.7071))

(define (rotate-down d [ang 90])
  (define-values (yaw pitch) (dir->angles d))
  (angles->dir yaw (- pitch ang)))

(module+ test
  (check-equal?
   (round-dir (rotate-down +x))
   -z))
(module+ test
  (round-dir (rotate-down -y 45))
  (dir 0.0 -0.7071 -0.7071))

(define (rotate-right d [ang 90])
  (rotate-around-dir
   d (rotate-up d) (- ang)))

(module+ test
  (check-equal?
   (round-dir (rotate-right +x))
   -y))
(module+ test
  (round-dir (rotate-right -y 45))
  (dir -0.7071 -0.7071 0.0))

(define (rotate-left d [ang 90])
  (rotate-around-dir
   d (rotate-up d) ang))

(module+ test
  (check-equal?
   (round-dir (rotate-left +x))
   +y))
(module+ test
  (round-dir (rotate-left -y 45))
  (dir 0.7071 -0.7071 0.0))
(module+ test
  (round-dir (rotate-right (rotate-left +x 45) 45))
  +x)