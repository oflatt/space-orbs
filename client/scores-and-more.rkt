#lang racket
(require pict3d pict3d-die-cut lens "structures.rkt" "current-roll-and-pos.rkt" "rotate-dir.rkt")
(provide make-cross draw-scores)


;;two strings -> frozen centered pict of that word
;;takes the word and color, and it does not center perfectly
(define (make-this-centered-word s c)
  (freeze
   (rotate-z
    (rotate-x
     (scale
      (move-z
       (with-emitted
        (emitted c 1.2)
        (die-cut-text s #:depth 4 #:top? true #:bottom? true #:sides? true #:center? true))
        2);;this is to center it based on the depth
      0.02)
     90)
    180)))

(define KILLS-DIECUT
  (make-this-centered-word "KILLS" "red"))

(define DEATHS-DIECUT
  (make-this-centered-word "DEATHS" "BLUE"))

(define CROSS-PICT
  (local
    ((define l (/ 1.5 16))
     (define h (/ l 7))
     (define hl (/ l 2))
     (define hh (/ h 2))
     (define gaph (/ l 6))
     (define half-cross
       (combine
        (rectangle
         (pos gaph (- hh) (- hh))
         (pos (+ hl h) hh hh));;right
        (rectangle
         (pos (- gaph) (- hh) (- hh))
         (pos (- (- hl) h) hh hh)))));;left
    (freeze
     (combine
      (rotate-x
       half-cross
       45)
      (rotate-z
       (rotate-y
        half-cross
        90)
       45)))))

;;orb and time-> pict
;;draws little cursor thing
(define (make-cross o t)
  (put-in-front o t CROSS-PICT))

;;orb and time -> pict
;;if tab is 
(define (draw-scores g t)
  (define o
    (lens-view
     game-orbs-player-lens
     g))
  (cond
    [(game-scores? g)
     (make-scoreboard o t)]
    [else
     empty-pict3d]))

;;pict-3d of scoreboard around origin
(define (make-scoreboard o t)
  (combine
   (put-in-front o t KILLS-DIECUT #:xoffset -6 #:yoffset -4)
   (put-in-front o t DEATHS-DIECUT  #:xoffset 6 #:yoffset -4)
   (put-in-front o t (make-this-centered-word (number->string (orb-kills o)) "red")  #:xoffset -6)
   (put-in-front o t (make-this-centered-word (number->string (orb-deaths o)) "blue")  #:xoffset 6)))

;;takes an orb, time, and a pict. Orients the pic in front of the orb
;;this assumes that the pict is centered on the origin perfectly
(define (put-in-front o t p #:xoffset (x 0) #:yoffset (y 0))
  (define cp (current-pos o t))
  (define cr (current-roll o t))
  (define up-dir (dir-normalize (rotate-up (orb-dir o) #:roll cr)))
  (define left-dir (dir-normalize (rotate-left (orb-dir o) #:roll cr)))
  (define pict-pos (pos+ cp (dir-scale (orb-dir o) 1)))
  (define-values (yaw pitch) (dir->angles (orb-dir o)))
  (move
   (move
    (rotate/center
     (rotate-z/center
      (rotate-x/center
       p
       (- pitch))
      (+ yaw 90))
     (orb-dir o)
     cr)
    (pos->dir pict-pos))
   (dir+ (dir-scale up-dir (- (/ y 10))) (dir-scale left-dir (- (/ x 10))))))