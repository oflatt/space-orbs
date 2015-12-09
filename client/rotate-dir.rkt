#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "structures.rkt")
(provide rotate-around-dir roll-to-dir dir-up-to-roll rotate-up rotate-down rotate-left rotate-right ang-between-dirs)

;;takes two dir and an angle -> one dir
;;rotates to-rotate around around, counterclockwise looking in the direction of around
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

;;takes two sets of coordinates and an angle -> dir
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

;;dir and angle -> dir
;;finds "up" relative to a dir and roll
;;I use the term roll for angle/twist or rotation of the camera
(define (roll-to-dir d roll)
  (rotate-up d #:roll roll))

;;two dir -> angle
;;gives the roll of a camera with the dir and a dir that points "up" from that dir
(define (dir-up-to-roll basedir updir)
  (define downdir (roll-to-dir basedir 0))
  (define leftdir (roll-to-dir basedir 90))
  (define a
    (*
     (asin (min 1.0 (/ (dir-dist (dir- downdir updir)) 2)))
     2
     (/ 180 pi)))
  (cond
    [(< (dir-dist (dir- leftdir updir)) (sqrt 2))
     a]
    [(= (- 360 a) 360)
     0]
    [else
     (- 360 a)]))

(module+ test
  (check-equal?
   (round (dir-up-to-roll +x +z))
   0.0))

;; two dir -> ang
;; gives the angle between two dirs
;; the two dirs should both be non-zero
(define (ang-between-dirs dir1 dir2)
  (define d1 (dir-normalize dir1))
  (define d2 (dir-normalize dir2))
  (*
   (asin (min 1.0 (/ (dir-dist (dir- d1 d2)) 2)))
   2
   (/ 180 pi)));;have to multiply by this because it is asin give radients


;;dir angle roll -> dir
;;rotates the dir "up" by the angle and around the dir by roll
;;finds "up" with a dir and roll
(define (rotate-up d [ang 90] #:roll [roll 0])
  (define-values (yaw pitch) (dir->angles d))
  (rotate-around-dir (angles->dir yaw (+ pitch ang)) d roll))

(module+ test
  (check-equal?
   (round-dir (rotate-up +x))
   +z))
(module+ test
  (check-equal?
   (round-dir (rotate-up -y 45))
   (dir 0.0 -0.7071 0.7071)))

;;dir angle roll -> dir
;;rotates the dir "down" by the angle and around the dir by roll
;;finds "down" with a dir and roll
(define (rotate-down d [ang 90] #:roll [roll 0])
  (define-values (yaw pitch) (dir->angles d))
  (rotate-around-dir (angles->dir yaw (- pitch ang)) d roll))

(module+ test
  (check-equal?
   (round-dir (rotate-down +x))
   -z))
(module+ test
  (check-equal?
   (round-dir (rotate-down -y 45))
   (dir 0.0 -0.7071 -0.7071)))

;;dir angle roll -> dir
;;rotates the dir "right" by the angle and around the dir by roll
;;finds "right" with a dir and roll
(define (rotate-right d [ang 90] #:roll [roll 0])
  (rotate-around-dir
   d (rotate-up d #:roll roll) (- ang)))

(module+ test
  (check-equal?
   (round-dir (rotate-right +x))
   -y))
(module+ test
  (check-equal?
   (round-dir (rotate-right -y 45))
   (dir -0.7071 -0.7071 0.0)))

;;dir angle roll -> dir
;;rotates the dir "left" by the angle and around the dir by roll
;;finds "left" with a dir and roll
(define (rotate-left d [ang 90] #:roll [roll 0])
  (rotate-around-dir
   d (rotate-up d #:roll roll) ang))

(module+ test
  (check-equal?
   (round-dir (rotate-left +x))
   +y))
(module+ test
  (check-equal?
   (round-dir (rotate-left -y 45))
   (dir 0.7071 -0.7071 0.0)))
(module+ test
  (check-equal?
   (round-dir (rotate-left +x #:roll 90))
   +z))
(module+ test
  (check-equal?
   (round-dir (rotate-right (rotate-left +x 45) 45))
   +x))