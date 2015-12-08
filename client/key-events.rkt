#lang racket
(require rackunit pict3d racket/set "structures.rkt" "current-roll-and-pos.rkt" "variables.rkt" "big-crunch.rkt" "on-frame.rkt")
(provide on-key on-release)

(define (on-key g n ot key)
  (define t (- ot MASTER-TIME-OFFSET))
  (define lkey (string-foldcase key))
  (define g*
    (struct-copy game g
                 [held-keys (set-add (game-held-keys g) lkey)]))
  (cond
    [(equal? key "escape")
     (struct-copy game g*
                  [exit? #t])]
    [(equal? key "\t")
     (struct-copy game g*
                  [scores? #t])]
    [(set-member? (game-held-keys g) lkey)
     ;; ignore it
     g]
    [(member lkey '("w" "a" "s" "d" " " "shift" "q" "e"))
     (struct-copy game g*
                  [orbs (on-orbs-key (game-orbs g*) n t key)]
                  [mt t])]
    [else g*]))

(define (on-orbs-key os n t key)
  (struct-copy orbs os
               [player (on-player-key (orbs-player os) n t key)]))

(define (on-player-key o n t key)
  (define lkey (string-foldcase key))
  (define result
    (on-player-key-result o n t lkey))
  (send-orb* result 'key t)
  result)

(define (on-player-key-result o n t lkey)
  (struct-copy orb o
               [pos (current-pos o t)]
               [time t]
               [movekeys (cons (movekey lkey STARTING-SPEED)  (orb-movekeys o))]
               [roll (current-roll o t)]))

(module+ test (check-equal? (on-player-key (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys empty]) "n" 5 "shift")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey "shift" STARTING-SPEED))])))

;#############################################################################################################################################################################################

(define (on-release g n ot key)
  (define t (- ot MASTER-TIME-OFFSET))
  (define lkey (string-foldcase key))
  (define g*
    (struct-copy game g [held-keys (set-remove (game-held-keys g) lkey)]))
  (cond
    [(equal? key "\t")
     (struct-copy game g*
                  [scores? #f])]
    [(member lkey '("w" "a" "s" "d" " " "shift" "q" "e"))
     (struct-copy game g*
                  [orbs (on-orbs-release (game-orbs g) n t key)]
                  [mt t])]
    [else g*]))

(define (on-orbs-release os n t key)
    (struct-copy orbs os
               [player (on-player-release (orbs-player os) n t key)]))

(define (on-player-release o n t key)
  (define lkey (string-foldcase key))
  (define result
    (on-player-release-result o n t lkey))
  (send-orb* result 'key-release t)
  result)

(define (on-player-release-result o n t lkey)
  (struct-copy orb
               o
               [pos (current-pos o t)]
               [time t]
               [movekeys (remove-this-movekey (orb-movekeys o) lkey)]
               [roll (current-roll o t)]))

(module+ test (check-equal? (on-player-release (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey " " 1))]) "n" 5 " ")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys empty])))

(define (remove-this-movekey ms key)
  (cond
    [(empty? ms) ms]
    [(equal? key (movekey-key (first ms)))
     (remove-this-movekey (rest ms) key)]
    [else
     (cons (first ms) (remove-this-movekey (rest ms) key))]))