#lang racket
(require "structures.rkt")

(provide stop-state?)

(define (stop-state? g n t)
  (game-exit? g))