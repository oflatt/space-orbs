#lang racket
(require pict3d lens rackunit "structures.rkt" "variables.rkt" "landscape.rkt" "current-roll-and-pos.rkt")
(provide on-frame send-orb* send-kill)

(define udps
  (udp-open-socket CLIENT-ADRESS PORT))

(udp-bind!
 udps
 #f
 0)

(udp-connect!
 udps
 CLIENT-ADRESS
 PORT)

(define (on-frame g n ot)
  (define t (- ot MASTER-TIME-OFFSET))
  (define cleaned (clean-old-shots g n t))
  (define with-received (on-receive cleaned n t))
  (define player (orbs-player (game-orbs with-received)))
  ;(collect-garbage #t)
  (cond
    [(send-orb g t)
     (struct-copy game with-received
                  [mt t]
                  [orbs
                   (struct-copy orbs (game-orbs with-received)
                                [player
                                 (struct-copy orb player
                                              [pos (current-pos player t)]
                                              [time t]
                                              [roll (current-roll player t)])])])]
    [else with-received]))

(define (clean-old-shots g n t)
  (lens-transform
   game-orbs-player-shots-lens
   g
   (λ (shots)
     (kill-old-shots shots t))))

;;sends if send speed time has passed
;;takes a game
(define (send-orb g t)
  (cond
    [(>= (- t (game-mt g)) UPDATE-SPEED)
     ;;(println (orbs-player (game-orbs g)))
     (send-state (convert-to-mypos (orbs-player (game-orbs g))))
     #t]
    [else #f]))

;;sends the message
(define (send-orb* o why t)
  (define converted (convert-to-mypos o))
  ; (printf "for ~a at ~a: ~s\n" why t o)
  (send-state converted))

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


(module+ test
  (check-equal?
   (convert-to-mypos TESTORB)
   (struct-copy orb TESTORB
                [pos
                 (mypos 1.0 1.0 1.0)]
                [dir
                 (mydir -1.0 0.0 0.0)])))
(module+ test
  (check-equal?
   (convert-to-mypos
    (struct-copy orb TESTORB
                 [shots
                  (list (shot (pos 20 20 0) (pos 0 0 0) (pos 1 1 1) 60 3 50))]))
   (struct-copy orb TESTORB
                [pos
                 (mypos 1.0 1.0 1.0)]
                [dir
                 (mydir -1.0 0.0 0.0)]
                [shots
                 (list (shot (mypos 20.0 20.0 0.0) (mypos 0.0 0.0 0.0) (mypos 1.0 1.0 1.0) 60 3 50))]
                )))

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
         (pos-z (shot-pos (first l))))]
       [corner1
        (mypos
         (pos-x (shot-corner1 (first l)))
         (pos-y (shot-corner1 (first l)))
         (pos-z (shot-corner1 (first l))))]
       [corner2
        (mypos
         (pos-x (shot-corner2 (first l)))
         (pos-y (shot-corner2 (first l)))
         (pos-z (shot-corner2 (first l))))])
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
         (mypos-z (shot-pos (first l))))]
       [corner1
        (pos
         (mypos-x (shot-corner1 (first l)))
         (mypos-y (shot-corner1 (first l)))
         (mypos-z (shot-corner1 (first l))))]
       [corner2
        (pos
         (mypos-x (shot-corner2 (first l)))
         (mypos-y (shot-corner2 (first l)))
         (mypos-z (shot-corner2 (first l))))])
      (shots-convert-to-pos (rest l)))]))

(module+ test
  (check-equal?
   (convert-to-pos
    (struct-copy orb TESTORB
                 [pos
                  (mypos 1 1 1)]
                 [dir
                  (mydir -1 0 0)]))
   TESTORB))
(module+ test
  (check-equal?
   (convert-to-pos
    (struct-copy orb TESTORB
                 [pos
                  (mypos 1 1 1)]
                 [dir
                  (mydir -1 0 0)]
                 [shots
                  (list (shot (mypos 20 20 0) (mypos 0 0 0) (mypos 1 1 1) 60 3 50))]))
   (struct-copy orb TESTORB
                [shots
                 (list (shot (pos 20 20 0) (pos 0 0 0) (pos 1 1 1) 60 3 50))])))

(define (convert-cubes-to-pos l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons
      (struct-copy mycube (first l)
                   [pos
                    (pos
                     (mypos-x (mycube-pos (first l)))
                     (mypos-y (mycube-pos (first l)))
                     (mypos-z (mycube-pos (first l))))])
      (convert-cubes-to-pos (rest l)))]))

(define (bytes->value bstr)
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
    [(equal? hostname #f)
     g]
    [(this-message? "kill" num-of-bytes)
     (lens-transform
      game-orbs-player-kills-lens
      g
      (lambda (kills)
        (+ 1 kills)))]
    [(this-message? "death" num-of-bytes)
     (lens-transform
      game-orbs-player-lens
      (lens-transform
       game-orbs-player-deaths-lens
       g
       (lambda (deaths)
         (+ 1 deaths)))
      (lambda (player)
        (respawn-orb player)))]
    [(this-message? "reset" num-of-bytes);;tells orb to reset milliseconds offset
     (set-offset t)
     (on-receive
      g
      n
      t)]
    [(this-message? "cubes" num-of-bytes);;gives cubes for landscape
     (set-cubes
      (convert-cubes-to-pos
       (message-data (bytes->value (subbytes byte-bucket 0 num-of-bytes)))))
     (on-receive
      g
      n
      t)]
    [(this-message? "new-connect" num-of-bytes);;gives a new orb
     ;(println "new-connect")
     (struct-copy game g
                  [orbs
                   (struct-copy orbs (game-orbs g)
                                [enemys
                                 (append
                                  (list-of-new-orbs
                                   (message-data (bytes->value
                                                  (subbytes byte-bucket 0 num-of-bytes))))
                                  (orbs-enemys (game-orbs g)))])])]
    [(this-message? "define" num-of-bytes);;defines the player's orb
     (define subm (bytes->value (subbytes byte-bucket 0 num-of-bytes)))
     (on-receive
      (struct-copy game g
                   [orbs
                    (struct-copy orbs (game-orbs g)
                                 [player
                                  (new-orb-from-define (message-data subm))])])
      n
      t)]
    [(this-message? "disconnect" num-of-bytes);;gives a orbdefine of a client that disconnected
     (define subm (bytes->value (subbytes byte-bucket 0 num-of-bytes)))
     (on-receive
      (lens-transform
       game-orbs-enemys-lens
       g
       (lambda (enemys)
         (delete-this-orb-from-define (message-data subm) enemys)))
      n
      t)]
    [else
     ; (printf "recv at ~a: ~s\n" t (bytes->value (subbytes byte-bucket 0 num-of-bytes)))
     (on-receive
      (struct-copy game g
                   [orbs
                    (struct-copy orbs (game-orbs g)
                                 [enemys
                                  (update-an-enemy
                                   (orbs-enemys (game-orbs g))
                                   (convert-to-pos
                                    (message-data
                                     (bytes->value (subbytes byte-bucket 0 num-of-bytes)))))])])
      n
      t)]))

(define (respawn-orb o)
  (struct-copy orb o
               [pos
                (cond
                  [(equal? (orb-color o) "red")
                   DEFAULTPOS2]
                  [else
                   DEFAULTPOS])]
               [movekeys empty]
               [dir DEFAULTDIR2]
               [roll 0]))

;;orbdefine and list of orbs -> list of orbs
(define (delete-this-orb-from-define od l)
  (cond
    [(empty? l)
     ;(println "error: got a delete for an orb I didn't have")
     l]
    [(and (equal? (orbdefine-port od) (orb-port (first l))) (equal? (orbdefine-hostname od) (orb-hostname (first l))))
     (rest l)]
    [else
     (cons (first l) (delete-this-orb-from-define od (rest l)))]))

;;takes a list and an orb -> list
(define (update-an-enemy l o)
  (cond
    [(empty? l)
     ;(println "error- message from unrecognized orb")
     empty]
    [(and (equal? (orb-hostname (first l)) (orb-hostname o)) (equal? (orb-port (first l)) (orb-port o)))
     (cons
      o
      (rest l))]
    [else
     (cons
      (first l)
      (update-an-enemy (rest l) o))]))

;;list of clients-> list of orbs
(define (list-of-new-orbs l)
  (cond
   [(empty? l)
    empty]
   [else
    (cons
     (struct-copy orb DEFAULT-ORB
                  [hostname (client-hostname (first l))]
                  [port (client-port (first l))])
     (list-of-new-orbs (rest l)))]))

;;orbdefine -> orb
(define (new-orb-from-define d)
  (cond
    [(equal? (orbdefine-color d) "blue")
     (struct-copy orb DEFAULT-ORB
                  [color (orbdefine-color d)]
                  [name (orbdefine-name d)]
                  [hostname (orbdefine-hostname d)]
                  [port (orbdefine-port d)]
                  [pos DEFAULTPOS]
                  [dir DEFAULTDIR])]
    [else
     (struct-copy orb DEFAULT-ORB
                  [color (orbdefine-color d)]
                  [name (orbdefine-name d)]
                  [hostname (orbdefine-hostname d)]
                  [port (orbdefine-port d)]
                  [pos DEFAULTPOS2]
                  [dir DEFAULTDIR2])]))

;;string and number of bytes -> bool
(define (this-message? s num)
  (equal? (message-name (bytes->value (subbytes byte-bucket 0 num))) s))

(define (value->bytes v)
  (define o (open-output-bytes))
  (write v o)
  (get-output-bytes o))

(define (send-state o)
  (udp-send
   udps
   (value->bytes (message "orb" o))))

;;sends a message with the orbdefine of the orb that was shot
(define (send-kill c)
  (udp-send
   udps
   (value->bytes (message "kill" c))))

;there is a copy of this in shots.rkt but it needs to require on frame
(define (kill-old-shots l t)
  (cond
    [(empty? l)
     empty]
    [(>= (- t (shot-time (first l))) SHOT-LIFE)
     (kill-old-shots (rest l) t)]
    [else
     (cons (first l) (kill-old-shots (rest l) t))]))