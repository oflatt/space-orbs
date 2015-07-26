#lang racket
(require pict3d "structures.rkt" "variables.rkt")
(provide on-frame send-orb)

(define udps
  (udp-open-socket "localhost" 50002))

(udp-bind!
 udps
 #f
 0)

(udp-connect!
 udps
 "localhost"
 50001)

(define (on-frame g n t)
  (define with-received (on-receive g n t))
  (cond
    [(send-orb g n t)
     (struct-copy game with-received
                  [mt t])]
    [else with-received]))

(define (send-orb g n t)
  (cond
    [(>= (- t (game-mt g)) SEND-SPEED)
     (send-state (convert-to-mypos (orbs-player (game-orbs g))))
     #t]
    [else #f]))

;;orb-> orb with mypos and mydir instead of pos and dir
(define (convert-to-mypos o)
  (struct-copy
   orb
   o
   [pos
    (mypos
     (pos-x (orb-pos o))
     (pos-y (orb-pos o))
     (pos-z (orb-pos o)))]
   [dir
    (mydir
     (dir-dx (orb-dir o))
     (dir-dy (orb-dir o))
     (dir-dz (orb-dir o)))]
   [shots
    (shots-convert-to-mypos (orb-shots o))]))

(define (shots-convert-to-mypos l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons
      (struct-copy
       shot
       (first l)
       [pos
        (mypos
         (pos-x (shot-pos (first l)))
         (pos-y (shot-pos (first l)))
         (pos-z (shot-pos (first l))))])
      (shots-convert-to-mypos (rest l)))]))

;;orb-> orb with mypos and mydir instead of pos and dir
(define (convert-to-pos o)
  (struct-copy
   orb
   o
   [pos
    (pos
     (mypos-x (orb-pos o))
     (mypos-y (orb-pos o))
     (mypos-z (orb-pos o)))]
   [dir
    (dir
     (mydir-dx (orb-dir o))
     (mydir-dy (orb-dir o))
     (mydir-dz (orb-dir o)))]
   [shots
    (shots-convert-to-pos (orb-shots o))]))

(define (shots-convert-to-pos l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons
      (struct-copy
       shot
       (first l)
       [pos
        (pos
         (mypos-x (shot-pos (first l)))
         (mypos-y (shot-pos (first l)))
         (mypos-z (shot-pos (first l))))])
      (shots-convert-to-pos (rest l)))]))
         
(define (bytes->value bstr)
  (println bstr)
  (define i (open-input-bytes bstr))
  (read i))

(define byte-bucket
  (make-bytes 20000))

;;returns a game
(define (on-receive g n t)
  (define-values (num-of-bytes hostname port)
    (udp-receive!*
     udps
     byte-bucket))
  (cond
    [(equal? num-of-bytes #f)
     g]
    [else
     (struct-copy game g
                  [orbs
                   (orbs
                    (orbs-player (game-orbs g))
                    (convert-to-pos (bytes->value (subbytes byte-bucket 0 num-of-bytes))))])]))

(define (value->bytes v)
  (define o (open-output-bytes))
  (write v o)
  (get-output-bytes o))

(define (send-state o)
  (udp-send
   udps
   (value->bytes o)))