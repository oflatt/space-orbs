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

;;takes a list of clients
(define (server-loop l)
  (define-values (num-of-bytes hostname port)
    (udp-receive!
     udps
     byte-bucket))
  (define r
    (take-out-of-list l (client hostname port)))
  (cond
    [(empty? l)
     (println "s")
     (server-loop (list (client hostname port)))]
    [(equal? (length l) (length r))
     "s2"
     (send-this (cons (client hostname port) l) (value->bytes "reset"))
     (server-loop (cons (client hostname port) l))]
    [else
     (send-this r (subbytes byte-bucket 0 num-of-bytes))
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