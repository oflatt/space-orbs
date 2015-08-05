#lang racket
(require pict3d rackunit "rotate-dir.rkt" "variables.rkt"  "structures.rkt" "landscape.rkt")
(provide move-with-collision)

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
  (cond
    [(collide? op mp poc+x)
     (try-to-slide op poc+x d sd+x dt L)]
    [(collide? op mp poc-x)
     (try-to-slide op poc-x d sd-x dt L)]
    [(collide? op mp poc+y)
     (try-to-slide op poc+y d sd+y dt L)]
    [(collide? op mp poc-y)
     (try-to-slide op poc-y d sd-y dt L)]
    [(collide? op mp poc+z)
     (try-to-slide op poc+z d sd+z dt L)]
    [(collide? op mp poc-z)
     (try-to-slide op poc-z d sd-z dt L)]
    [else mp]))

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
(define (try-to-slide op poc d sd dt L)
  (define sp (slide-against-surface op poc d sd dt))
  (define slide-dir (rotate-around-dir (dir-cross d sd) sd 90))
  (define-values (posc ssd) (trace/normal L op slide-dir));;slide surface data
  (define sp2 (slide-against-surface op posc d ssd dt))
  (define slide-dir2 (rotate-around-dir (dir-cross d ssd) ssd 90))
  (define posc2 (trace L op slide-dir2))
  (cond
    [(equal? posc #f)
     sp]
    [(not (collide? op sp posc))
     sp]
    [(not (collide? op sp2 posc2))
     sp2]
    [else op]))

;old position, point of collision, dir, and surface dir-> new pos
(define (slide-against-surface op poc d sd dt)
  (define slide-dir (dir-normalize (rotate-around-dir (dir-cross d sd) sd 90)))
  (define slide-speed
    (cond
      [(> (dir-up-to-roll d sd) 5)
       SLIDE-SPEED]
      [else 0]))
  (pos+ op (dir-scale slide-dir (* dt slide-speed))))