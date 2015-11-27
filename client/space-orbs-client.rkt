#lang racket
(require pict3d "variables.rkt" "on-draw.rkt" "key-events.rkt" "on-mouse.rkt" "stop-state.rkt" "big-crunch.rkt" "on-frame.rkt"
         "frame-handling.rkt"
         racket/class
         racket/gui/base)

(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 1
                            #:roughness 0.2))

(define gl-config (new gl-config%))
(send gl-config set-sync-swap #t)
(send gl-config set-legacy? #f)

(big-bang3d/big-crunch
 DEFAULT-STATE
 #:name "Space Orbs"
 #:on-draw on-draw
 #:on-key on-key
 #:on-release on-release
 #:on-mouse on-mouse
 #:stop-state? stop-state?
 #:on-frame on-frame
 #:display-mode 'hide-menu-bar
 #:gl-config gl-config
 #:cursor (make-cursor-blank))