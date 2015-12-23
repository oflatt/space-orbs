#lang racket
(require "structures.rkt" "on-frame.rkt")

(provide stop-state?)

(define (stop-state? g n t)
  (cond
    [(game-exit? g)
     (send-this
      "bye" 0)])
  (game-exit? g))