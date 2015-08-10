#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "current-roll-and-pos.rkt" "variables.rkt" "draw-enemys.rkt" "on-frame.rkt" "landscape.rkt")
(provide shot-pics new-shot kill-old-shots on-shoot)

(define (on-shoot os t)
  (define killc (shot-orb os t));;killed client
  (define with-new (add-shot os t))
  (cond
    [killc
     (send-kill killc)
     with-new]
    [else
     with-new]))

(define (add-shot os t)
  (define o (orbs-player os))
  (define poc (trace
               (combine
                FINAL-LANDSCAPE
                (draw-enemys (orbs-enemys os) t))
               (current-pos o t)
               (orb-dir o)))
  (struct-copy
   orb
   o
   [shots
    (cond
      [(equal? poc #f)
       (orb-shots o)]
      [else (cons (new-shot o poc t) (orb-shots o))])]))

;;takes an orbs and time and gives a client
(define (shot-orb os t)
  (shot-orb-helper (current-pos (orbs-player os) t) (orb-dir (orbs-player os)) (orbs-enemys os) t))

;;takes a pos, dir, and a list of enemy orbs
(define (shot-orb-helper pos dir l t)
  (cond
    [(empty? l)
     #f]
    [else
     (shot-orb-helper-not-empty pos dir l t)]))

;;because I want to define poc, the list has to not be empty
(define (shot-orb-helper-not-empty pos dir l t)
  (define o (first l))
  (define poc (trace (draw-enemy o t) pos dir))
  (cond
    [poc
     (client (orb-hostname o) (orb-port o))]
    [else
     (shot-orb-helper pos dir (rest l) t)]))
     
  
;;takes an orbs -> list of pict3ds for all the shots in it
(define (shot-pics os t)
  (shot-pics-from-list
   (cons
    (orbs-player os)
    (orbs-enemys os))
   t))

(define (shot-pics-from-list l t)
  (cond
    [(empty? l)
     empty]
    [else
     (append
      (get-shot-pics-from-orb (first l) t)
      (shot-pics-from-list (rest l) t))]))

(define (get-shot-pics-from-orb o t)
  (get-pics-from-shots (orb-shots o) t (orb-color o)))

(define (get-pics-from-shots l t c)
  (cond
    [(empty? l)
     empty]
    [else
     (cons
      (draw-shot (first l) t c)
      (get-pics-from-shots (rest l) t c))]))

(define (draw-shot s t c)
  (define yaw (shot-yaw s))
  (define pitch (shot-pitch s))
  (define p (shot-pos s))
  (with-emitted
   (emitted
    c
    (-
     2
     (*
      (/
       (- t (shot-time s))
       SHOT-LIFE)
      1.5)))
   (move
    (rotate-z
     (rotate-y
      (cylinder
       (shot-corner1 s)
       (shot-corner2 s))
      (+ 90 (- pitch)))
     yaw)
    (dir (pos-x p) (pos-y p) (pos-z p)))))

(define (new-shot o poc t)
  (define cp (current-pos o t))
  (define-values (yaw pitch) (dir->angles (orb-dir o)))
  (define center (pos-between (current-pos o t) poc 1/2))
  (define h (/ SHOT-WIDTH 2))
  (shot
   (pos
    (- h)
    (- h)
    (- (- (/ (pos-dist cp poc) 2) 1)))
   (pos
    h
    h
    (- (/ (pos-dist cp poc) 2) 1))
   center
   yaw
   pitch
   t))

;list of shots and time-> list of shots
(define (kill-old-shots l t)
  (cond
    [(empty? l)
     empty]
    [(>= (- t (shot-time (first l))) SHOT-LIFE)
     (kill-old-shots (rest l) t)]
    [else
     (cons (first l) (kill-old-shots (rest l) t))]))
