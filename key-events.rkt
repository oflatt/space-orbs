#lang racket
(require rackunit pict3d "structures.rkt" "current-ang-and-pos.rkt" "variables.rkt" "big-crunch.rkt")
(provide on-key on-release)

(define (find-this-movekey key ms)
  (cond
    [(empty? ms)
     #f]
    [(equal? (movekey-key (first ms)) key)
     (first ms)]
    [else (find-this-movekey key (rest ms))]))

(module+ test (check-equal? (find-this-movekey "w" '())
                           #f))
(module+ test (check-equal? (find-this-movekey "a" (list (movekey "w" 1/2) (movekey "a" 1/2)))
                            (movekey "a" 1/2)))

(define (on-key g n t key)
  (cond
    [(equal? key "escape")
     (struct-copy game g
                  [exit? #t])]
    [else
     (struct-copy game g
                  [orbs (on-orbs-key (game-orbs g) n t key)])]))

(define (on-orbs-key os n t key)
  (struct-copy orbs os
               [player (on-player-key (orbs-player os) n t key)]))

(define (on-player-key o n t key)
  (define lkey (string-foldcase key))
  (cond
    [(find-this-movekey lkey (orb-movekeys o))
     o]
    [(or (equal? lkey "w") (equal? lkey "s") (equal? lkey "a") (equal? lkey "d") (equal? lkey " ") (equal? lkey "shift") (equal? lkey "q") (equal? lkey "e"))
     (struct-copy orb o
                  [pos (current-pos o t)]
                  [time t]
                  [movekeys (cons (movekey lkey STARTING-SPEED)  (orb-movekeys o))]
                  [ang (current-ang o t)])]
    [else o]))

(module+ test (check-equal? (on-player-key (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey " " 1))]) "n" 30 " ")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey " " 1))])))

(module+ test (check-equal? (on-player-key (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys empty]) "n" 5 "shift")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey "shift" STARTING-SPEED))])))

;#############################################################################################################################################################################################

(define (on-release g n t key)
    (struct-copy game g
               [orbs (on-orbs-release (game-orbs g) n t key)]))

(define (on-orbs-release os n t key)
    (struct-copy orbs os
               [player (on-player-release (orbs-player os) n t key)]))

(define (on-player-release o n t key)
  (define lkey (string-foldcase key))
  (struct-copy orb
               o
               [pos (current-pos o t)]
               [time t]
               [movekeys (remove-this-movekey (orb-movekeys o) lkey)]
               [ang (current-ang o t)]))

(module+ test (check-equal? (on-player-release (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey " " 1))]) "n" 5 " ")
              (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys empty])))

(define (remove-this-movekey ms key)
  (cond
    [(empty? ms) ms]
    [(equal? key (movekey-key (first ms)))
     (remove-this-movekey (rest ms) key)]
    [else
     (cons (first ms) (remove-this-movekey (rest ms) key))]))