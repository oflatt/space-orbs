#lang racket
(require pict3d rackunit "rotate-dir.rkt" "variables.rkt"  "structures.rkt" "landscape.rkt")
(provide move-with-collision move-with-collision*)

;; original position, velocity, delta-time, and a landscape-pict3d -> moved position with collisions accounted for
;; collisions are complety inelastic for now
(define (move-with-collision* op v dt L)
  (move-with-collision op (pos+ op (dir-scale v dt)) v dt L))

;;original position, moved position, a dir, delta time, and a landscape/pict3d-> moved position with any collisions accounted for
;;poc is pos of collision and sd is surface dir, the dir perpendicular to the surface
;L is the landscape
(define (move-with-collision op mp d dt L)
  (define-values (poc sd) (trace/normal L op d))
  (define-values (poc+x sd+x) (trace/normal L op +x))
  (define-values (poc-x sd-x) (trace/normal L op -x))
  (define-values (poc+y sd+y) (trace/normal L op +y))
  (define-values (poc-y sd-y) (trace/normal L op -y))
  (define-values (poc+z sd+z) (trace/normal L op +z))
  (define-values (poc-z sd-z) (trace/normal L op -z))
  (define l;;list of lists with a poc and the matching sd in it
    (get-list-of-collisions
     op
     mp
     (list
      (list poc+x sd+x)
      (list poc-x sd-x)
      (list poc+y sd+y)
      (list poc-y sd-y)
      (list poc+z sd+z)
      (list poc-z sd-z))))
  (cond
    [(empty? l)
     mp]
    [(equal? (length l) 2)
     (slide-against-corner op d dt (second (first l)) (second (second l)))]
    [(> (length l) 2)
     op]
    [else
     (try-to-slide op (first (first l)) d (second (first l)) dt L)]))

;;old pos, moved pos, and a list of lists containing a point of contact and surface data-> list of lists that had collisions
(define (get-list-of-collisions op mp l)
  (cond
    [(empty? l)
     empty]
    [(collide? op mp (first (first l)))
     (cons
      (first l)
      (get-list-of-collisions op mp (rest l)))]
    [else
     (get-list-of-collisions op mp (rest l))]))

;;takes a old pos, a dir, a delta time, and two surface datas
(define (slide-against-corner op d dt sd1 sd2)
  (define slide-dir (dir-cross sd1 sd2))
  (define slide-dir-opposite (dir-cross sd2 sd1))
  (define sp (pos+ op (dir-scale slide-dir (* dt SLIDE-SPEED))));;slide pos using slide speed and time
  (define sp-o (pos+ op (dir-scale slide-dir-opposite (* dt SLIDE-SPEED))));;opposite direction slide pos
  (define sd-ang (ang-between-dirs d slide-dir))
  (define sd-ang-o (ang-between-dirs d slide-dir-opposite))
  (cond
    [(< sd-ang sd-ang-o);;if the dir is closer to the original slide dir than the opposite of it (so it slides in the right direction)
     sp]
    [else
     sp-o]))

;;list -> list of all items that were true
(define (get-true l)
  (cond
    [(empty? l)
     empty]
    [(first l)
     (cons (first l) (get-true (rest l)))]
    [else
     (get-true (rest l))]))

;list of bools-> how many of them are true
(define (number-of-true l)
  (cond
    [(empty? l)
     0]
    [(first l)
     (add1 (number-of-true (rest l)))]
    [else
     (number-of-true (rest l))]))

;;true if poc is close enough for a collision to happen
(define (collide? op mp poc)
  (cond
    [(equal? poc #f)
     #f]
    [else
     (or (<= (pos-dist mp poc) ORB-RADIUS) (<= (pos-dist op poc) (pos-dist op mp)))]))

(module+ test
  (check-equal?
   (move-with-collision origin (pos 1 0 0) +x 1 TEST-LAND)
   (pos 1 0 0)))
(module+ test
  (define test (round-pos (move-with-collision (pos 0 9.5 0) (pos 20.0 -20 0.7071) (dir 0.7071 0.7071 0.0) 1 TEST-LAND)))
  (check-equal?
   (cond
     [(equal? (pos-z test) -0.0)
      (pos (pos-x test) (pos-y test) 0)]
     [else test])
   (round-pos (pos SLIDE-SPEED 9.5 0))))

;sp is a slided pos and posc is point of slide collision
;;takes old pos, point of collision, dir, surface data, delta time, and landscape
(define (try-to-slide op poc d sd dt L)
  (define sp (slide-against-surface op d sd dt 90))
  (define slide-dir (rotate-around-dir (dir-cross d sd) sd 90))
  (define-values (posc ssd) (trace/normal L op slide-dir));;slide surface data
;  (define sp2 (slide-against-surface op sd ssd dt 0))
;  (define slide-dir2 (dir-cross sd ssd))
;  (define posc2 (trace L op slide-dir2))
  (cond
    [(equal? posc #f)
     sp]
    [(not (collide? op sp posc));;fixme, approaching corner then bounce back into ground
     sp]
;    [(not (collide? op sp2 posc2)) always slide on corner
;     sp2]
    [else op]))

;old position, point of collision, dir, surface dir, and rotation-> new pos
(define (slide-against-surface op d sd dt r)
  (define slide-dir (dir-normalize (rotate-around-dir (dir-cross d sd) sd r)))
  (define slide-speed
    (cond
      [(> (dir-up-to-roll d sd) 5)
       SLIDE-SPEED]
      [else 0]))
  (pos+ op (dir-scale slide-dir (* dt slide-speed))))