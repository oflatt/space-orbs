#lang racket
(require pict3d rackunit "frame-handling.rkt" "structures.rkt" "current-roll-and-pos.rkt" "variables.rkt" "landscape.rkt")
(provide on-draw)

(define MAX-SCREEN 'beginning);maximize only works if the screen has already been drawn once

(define (on-draw g n t)
  (on-orbs-draw (game-orbs g) n t))

(define (on-orbs-draw os n t)
  (define p (orbs-player os))
  (define draw
    (combine
     FINAL-LANDSCAPE
     (apply combine (list-of-shot-pictures os t))
     (draw-enemy (orbs-enemy os) t)
     (lights+camera (current-pos p t) (orb-dir p) (current-roll p t))))
  (cond
    [(equal? MAX-SCREEN 'beginning)
     (set! MAX-SCREEN 'ready)
     draw]
    [(equal? MAX-SCREEN 'ready)
     (maximize-screen)
     (make-cursor-blank)
     (set! MAX-SCREEN 'done)
     draw]
    [else draw]))

(define (list-of-shot-pictures os t)
  (define player-shots (kill-old-shots (orb-shots (orbs-player os)) t))
  (define enemy-shots (kill-old-shots (orb-shots (orbs-enemy os)) t))
  (append
   (get-pics-from-shots
    player-shots t (orb-color (orbs-player os)))
   (get-pics-from-shots
    enemy-shots t (orb-color (orbs-enemy os)))))

;list of shots and time-> list of shots
(define (kill-old-shots l t)
  (cond
    [(empty? l)
     empty]
    [(>= (- t (shot-time (first l))) SHOT-LIFE)
     (kill-old-shots (rest l) t)]
    [else
     (cons (first l) (kill-old-shots (rest l) t))]))

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
  
(define (draw-enemy o t)
  (sphere (current-pos o t) 2))

(define (lights+camera currentpos d ang)
  (combine (apply combine
                  (cond
                    [DISCO?
                     (pick-random-lights empty NUM-OF-LIGHTS)]
                    [else LIGHTS-LIST]))
           (basis 'camera (point-at
                           currentpos
                           d
                           #:angle ang))))