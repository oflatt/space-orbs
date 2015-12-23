#lang racket
(require "../client/structures.rkt" "../client/variables.rkt" rackunit)

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

;;takes a server-game
(define (server-loop g)
  (define-values (num-of-bytes hostname port)
    (udp-receive!
     udps
     byte-bucket))
  (define sender-client (client hostname port (current-milliseconds)))
  (define m (subbytes byte-bucket 0 num-of-bytes))
  (define mvalue (bytes->value m))
  (define l (server-game-orbs g))
  (define cl (get-clients g))
  (define (recur x)
            (server-loop x))
  (cond
    [(equal? mvalue "bad message!")
     (recur g)]
    [(equal? (message-name mvalue) "hello")
     (define client-server-orb (new-server-orb sender-client g))
     (println (+ 1 (length l)))
     (send-this (list sender)
                (value->bytes (message "cubes" CUBE-LIST)))
     (send-this (list sender)
                (value->bytes (message "define" (server-orb->orbdefine client-server-orb g))))
     (send-this (list sender)
                (value->bytes (message "new-connect" (server-orb-list->orbdefine-list (server-game orbs)))))
     (send-this cl
                (value->bytes (message "new-connect" (server-orb->orbdefine client-server-orb g))))
     (recur (cons client-server-orb g))]
    [else
     (recur g)]))

;;client and a server-game -> server-orb
(define (new-server-orb c g)
  (... ;;check the map first to see how many teams

;;server-game, client, and a string for the name -> game with the client added as a new orb
(define (add-orb g c n)
  (lens-transform
   server-game-orbs-lens
   g
   (lambda (l)
     (cons
      (server-orb n c 0 0)
      l))))

;;takes a list of clients
(define (old-server-loop old-l)
  (define-values (num-of-bytes hostname port)
    (udp-receive!
     udps
     byte-bucket))
  (define sender (client hostname port (current-milliseconds)))
  (define l (replace-client old-l sender))
  (define r
    (take-client-out-of-list l sender))
  (define m (subbytes byte-bucket 0 num-of-bytes))
  (define mvalue (bytes->value m))
  (define oc? (old-client? l))
  (cond
    [(empty? l)
     (println 1)
     (send-this (list sender)
                (value->bytes (message "cubes" CUBE-LIST)))
     (send-this (list sender)
                (value->bytes (message "define" (this-orbdefine sender empty))))
     (server-loop (list sender))]
    [(equal? (length l) (length r));;If it is a new orb connecting
     (define clean-l
       (cond
         [oc?
          (take-client-out-of-list l oc?)]
         [else
          l]))
     (define sender-orbdefine
       (this-orbdefine sender clean-l))
     (send-this clean-l
                (value->bytes (message "new-connect" (list sender))))
     (send-this (list sender)
                (value->bytes (message "new-connect" clean-l)))
     (send-this (list sender)
                (value->bytes (message "cubes" CUBE-LIST)))
     (send-this (list sender)
                (value->bytes (message "define" sender-orbdefine)))
     (cond
       [oc?
        (println (length l))
        (server-loop (cons sender clean-l))]
       [else
        (println (+ 1 (length l)))
        (server-loop (cons sender clean-l))])]
    [(equal? mvalue "bad message!")
     (server-loop l)]
    [(equal? (message-name mvalue) "kill")
     (send-this (list (client-from-orbdefine (message-data mvalue)))
                (value->bytes (message "death" "respawn")))
     (send-this (list sender)
                (value->bytes (message "kill" 1)))
     (server-loop l)]
    [oc?
     (send-this l (value->bytes (message "disconnect" (this-orbdefine oc? empty))))
     (send-this r m)
     (println (- (length l) 1))
     (server-loop  (take-client-out-of-list l oc?))]
    [else
     (send-this r m)
     (server-loop l)]))

(define (client-from-orbdefine od)
  (client (orbdefine-hostname od) (orbdefine-port od) 1))

;;list of clients and a client-> list of clients
(define (replace-client l s)
  (cond
    [(empty? l)
     empty];;if it is a new client it won't already be in the list
    [(same-client? (first l) s)
     (cons
      s
      (rest l))]
    [else
     (cons
      (first l)
      (replace-client (rest l) s))]))

(define (same-client? c1 c2)
  (and (equal? (client-port c1) (client-port c2)) (equal? (client-hostname c1) (client-hostname c2))))

;;list of clients -> client or false
(define (old-client? l)
  (cond
    [(empty? l)
     false]
    [(> (- (current-milliseconds) (client-last-message-time (first l))) 1000);;if it has been this long since the last message
     (first l)]
    [else
     (old-client? (rest l))]))

(define (this-orbdefine s clients)
  (define n (length clients))
  (orbdefine
   (number->string (+ 1 n))
   (cond
     [(even? n)
      "blue"]
     [else
      "red"])
   (client-hostname s)
   (client-port s)))

;;takes a list of clients and a bstr and sends it to all the clients
(define (send-this l b)
  (cond
    [(empty? l)
     void]
    [else
     (udp-send-to udps (client-hostname (first l)) (client-port (first l)) b)
     (send-this (rest l) b)]))

;;takes one client out of the list
(define (take-client-out-of-list l s)
  (cond
    [(empty? l)
     empty];;if client was not in list at all
    [(same-client? (first l) s)
     (rest l)]
    [else
     (cons
      (first l)
      (take-client-out-of-list (rest l) s))]))

(define (value->bytes v)
  (define o (open-output-bytes))
  (write v o)
  (get-output-bytes o))

(define (bytes->value bstr)
  (define i (open-input-bytes bstr))
  (with-handlers ([exn:fail:read? (lambda (x) "bad message")]);;if it is a bad message
    (read i)))

;;server-game -> list of clients
(define (get-clients g)
  (get-clients-helper (server-game-orbs)))

;;list of server-orbs -> list of clients
(define (get-clients-helper l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons (server-orb-client (first l)) (get-clients-helper (rest l)))]))

(server-loop DEFAULT-SERVER)