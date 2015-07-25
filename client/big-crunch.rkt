#lang racket/base
(require pict3d/universe
         (only-in racket/gui/base
                  current-eventspace
                  make-eventspace
                  queue-callback))

(provide big-bang3d/big-crunch)

(define (call-with-big-crunch thunk)
  (define clean-up (make-custodian))
  (define ch (make-channel))
  (parameterize ([current-custodian clean-up])
    (parameterize ([current-eventspace (make-eventspace)])
      (queue-callback
       (lambda ()
         (channel-put ch (thunk))))))
  (begin0
   (channel-get ch)
   (custodian-shutdown-all clean-up)))

(define-syntax-rule (big-bang3d/big-crunch state clause ...)
  (call-with-big-crunch
   (lambda ()
     (big-bang3d state clause ...))))
