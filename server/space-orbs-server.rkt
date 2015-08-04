#lang racket
(require "../client/structures.rkt" "../client/variables.rkt")

(define udps
  (udp-open-socket SERVER-ADRESS PORT))

(udp-bind!
 udps
 SERVER-ADRESS
 PORT
 #t)

(define byte-bucket
  (make-bytes PORT))

;list of cubes and number of cubes to make-> list of cubes
(define (pick-random-cubes l n)
  (define random-cube
    (mycube (mypos (random (+ 1 (* WALL-SIZE 2))) (random (+ 1 WALL-SIZE)) (random (+ WALL-SIZE 1))) (+ 2 (random) (random)) "white"))
  (cond
    [(= n 0)
     l]
    [else
     (pick-random-cubes (cons random-cube l) (- n 1))]))

(define CUBE-LIST (pick-random-cubes empty 25))

;;takes a list of clients
(define (server-loop l)
  (define-values (num-of-bytes hostname port)
    (udp-receive!
     udps
     byte-bucket))
  (define r
    (take-out-of-list l (client hostname port)))
  (define sender (client hostname port))
  (cond
    [(empty? l)
     ;(println "s")
     (send-this (list sender)
                (value->bytes (message "cubes" CUBE-LIST)))
     (server-loop (list sender))]
    [(equal? (length l) (length r))
     (send-this (cons sender l)
                (value->bytes (message "reset" 0)))
     (send-this (list sender)
                (value->bytes (message "cubes" CUBE-LIST)))
     (server-loop (cons sender l))]
    [else
     (send-this r
                (subbytes byte-bucket 0 num-of-bytes))
     (server-loop l)]))

;;takes a list of clients and a bstr and sends it to all the clients
(define (send-this l b)
  (cond
    [(empty? l)
     void]
    [else
     (udp-send-to udps (client-hostname (first l)) (client-port (first l)) b)
     (send-this (rest l) b)]))

(define (take-out-of-list l s)
  (cond
    [(empty? l)
     empty]
    [(equal? s (first l))
     (rest l)]
    [else
     (cons
      (first l)
      (take-out-of-list (rest l) s))]))

(define (value->bytes v)
  (define o (open-output-bytes))
  (write v o)
  (get-output-bytes o))

(define (bytes->value bstr)
  (define i (open-input-bytes bstr))
  (read i))

(server-loop empty)