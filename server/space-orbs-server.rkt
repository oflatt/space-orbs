#lang racket
(require "../client/structures.rkt")

(define udps
  (udp-open-socket "localhost" 50001))

(udp-bind!
 udps
 "localhost"
 50001
 #t)

(define byte-bucket
  (make-bytes 20000))

;;takes a list of clients
(define (server-loop cs)
  (define-values (num-of-bytes hostname port)
    (udp-receive!
     udps
     byte-bucket))
  (define r
    (take-out-of-list cs (client hostname port)))
  (cond
    [(equal? (length cs) (length r))
     (server-loop
      (cons
       (client hostname port)
       cs))]
    [else
     (send-this r (subbytes byte-bucket 0 num-of-bytes))
     (server-loop cs)]))

;;takes a list of clients and a bstr and sends it to all the clients
(define (send-this cs b)
  (cond
    [(empty? cs)
     void]
    [else
     (udp-send-to udps (client-hostname (first cs)) (client-port (first cs)) b)
     (send-this (rest cs) b)]))

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