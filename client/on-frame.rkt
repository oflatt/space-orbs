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

;;this on-frame checks if it is not connected and tries connect if it is not
(define (on-frame g n ot)
  (define t (- ot MASTER-TIME-OFFSET))
  (define c? (game-not-connected? g))
  (cond
    [c?
     (cond
       [(>= (- t c?) SPEED-OF-HELLO)
        (udp-send
         udps
         (value->bytes (message "hello" (orb-name (game-player g)))))])])
   (on-frame-helper g n t))

(define (on-frame-helper g n t)
  (define cleaned (clean-old-shots g n t))
  (define with-received (on-receive cleaned n t))
  (define player (game-player with-received))
  ;(collect-garbage #t)
  (cond
    [(>= (- t (game-mt g)) UPDATE-SPEED)
     (define updated-game
       (update-game with-received t))
     (send-orb updated-game)
     updated-game]
    [else with-received]))

(define (clean-old-shots g n t)
  (lens-transform
   game-player-shots-lens
   g
   (λ (shots)
     (kill-old-shots shots t))))

;;re-freashes the pos, time, and roll of the player
;;makes mt t to signify it has refreashed
(define (update-game g t)
  (define player (game-player g))
  (struct-copy game g
                  [mt t]
                  [player
                   (struct-copy orb player
                                [pos (current-pos player t)]
                                [time t]
                                [roll (current-roll player t)])]))

;;takes a game and sends the player orb to server
(define (send-orb g)
  ;;(println (orbs-player (game-orbs g)))
  (cond
    [(game-not-connected? g)
     void]
    [else
     (send-state (convert-to-mypos (game-player g)))]))

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
    (pos->mypos (orb-pos o))]
   [dir
    (dir->mydir (orb-dir o))]
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
  (map shot-convert-to-mypos l))

(define (shot-convert-to-mypos s)
  (struct-copy
   shot
   s
   [pos
    (pos->mypos (shot-pos s))]
   [corner1
    (pos->mypos (shot-corner1 s))]
   [corner2
    (pos->mypos (shot-corner2 s))]))

;;orb-> orb with mypos and mydir instead of pos and dir
(define (convert-to-pos o)
  (struct-copy
   orb
   o
   [pos
    (mypos->pos (orb-pos o))]
   [dir
    (mydir->dir (orb-dir o))]
   [shots
    (shots-convert-to-pos (orb-shots o))]))

(define (shots-convert-to-pos l)
  (map shot-convert-to-pos l))

(define (shot-convert-to-pos s)
  (struct-copy
   shot
   s
   [pos
    (mypos->pos (shot-pos s))]
   [corner1
    (mypos->pos (shot-corner1 s))]
   [corner2
    (mypos->pos (shot-corner2 s))]))

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
  (map convert-cube-to-pos l))

(define (convert-cube-to-pos cube)
  (struct-copy mycube cube
               [pos
                (mypos->pos (mycube-pos cube))]))

(define (bytes->value bstr)
  (define i (open-input-bytes bstr))
  (read i))

(define byte-bucket
  (make-bytes 20000))

;;returns a game
;;These are the messages
;;"hello"- message from client trying to connect to the server for the first time containing the name of that orb
;;"kill"- message from client with the orbdefine of the orb it killed
;;"add kill"- message from server with a number of how many kills to add to kills
;;"add death"- message from server with number of how many deaths to add to deaths, also automatically respawns orb
;;"reset time"- message from server telling clients to sync their time by setting the offset to current time
;;"server landscape"- message from server with a server landscape
;;"new connect"- message from server with a list of orb defines to add
;;"define"- message from server defining the client it is sent to, officially connects the client to server
;;"disconnect"- message from server telling an orb that disconnected
;;"bye"- message from client that says it is disconnecting
(define (on-receive g n t)
  (define-values (num-of-bytes hostname port)
    (udp-receive!*
     udps
     byte-bucket))
  (define gamemode (game-mode g))
  (define (recur x)
    (on-receive x n t))
  (cond
    [(equal? hostname #f)
     g]
    [(this-message? "add kill" num-of-bytes)
     (recur
         (lens-transform
          game-player-kills-lens
          g
          (lambda (kills)
            (+
             (message-data (bytes->value
                            (subbytes byte-bucket 0 num-of-bytes)))
             kills))))]
    [(this-message? "add death" num-of-bytes)
     (recur
         (lens-transform/list
          g
          game-player-deaths-lens
          (lambda (deaths)
            (+
             (message-data (bytes->value
                            (subbytes byte-bucket 0 num-of-bytes)))
             deaths))
          game-player-lens
          (lambda (player)
            (respawn-orb player))))]
    [(this-message? "reset time" num-of-bytes);;tells orb to reset milliseconds offset
     (set-offset t)
     (recur g)]
    [(this-message? "server landscape" num-of-bytes);;gives a server-landscape
     (define sl
       (message-data (bytes->value (subbytes byte-bucket 0 num-of-bytes))))
     (cond
       [(equal? (server-landscape-name sl) 'cubes)
        (set-cubes
         (convert-cubes-to-pos
          (server-landscape-data sl)))]
       [(equal? (server-landscape-name sl) "text")
        void];;FIXME, make server landscape variable
       [else
        (println "error: got an unrecognized server-landscape name")])
     (recur g)]
    [(this-message? "new-connect" num-of-bytes);;gives a list of new orb
     (define new-orbs
       (list-of-new-orbs
        (message-data (bytes->value
                       (subbytes byte-bucket 0 num-of-bytes)))))
     (recur
         (simple-change-enemys
          g
          (lambda (enemys)
            (append new-orbs enemys))))]
    [(this-message? "define" num-of-bytes);;defines the player's orb
     (define subm (bytes->value (subbytes byte-bucket 0 num-of-bytes)))
     (recur
         (lens-set
          game-not-connected?-lens
          (lens-set
           game-player-lens
           g
           (new-orb-from-define (message-data subm)))
          #f))]
    [(this-message? "disconnect" num-of-bytes);;gives a orbdefine of a client that disconnected
     (define subm (bytes->value (subbytes byte-bucket 0 num-of-bytes)))
     (recur
       (simple-change-enemys
        g
        (lambda (enemys)
          (delete-this-orb-from-define (message-data subm) enemys))))]
    [else
     ; (printf "recv at ~a: ~s\n" t (bytes->value (subbytes byte-bucket 0 num-of-bytes)))
     (recur
      (simple-change-enemys
       g
       (lambda (enemys)
         (update-an-enemy
          enemys
          (convert-to-pos
           (message-data
            (bytes->value (subbytes byte-bucket 0 num-of-bytes))))))))]))

(define (respawn-orb o)
  (struct-copy orb o
               [pos
                (cond
                  [(equal? (orb-color o) "red")
                   DEFAULTPOS2]
                  [else
                   DEFAULTPOS])]
               [movekeys empty-movekeys]
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

;;string and data to send
(define (send-this n d)
  (udp-send
   udps
   (value->bytes (message n d))))

;there is a copy of this in shots.rkt but it needs to require on frame
(define (kill-old-shots l t)
  (cond
    [(empty? l)
     empty]
    [(>= (- t (shot-time (first l))) SHOT-LIFE)
     (kill-old-shots (rest l) t)]
    [else
     (cons (first l) (kill-old-shots (rest l) t))]))