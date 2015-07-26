#lang racket
(require pict3d "variables.rkt" "on-draw.rkt" "key-events.rkt" "on-mouse.rkt" "stop-state.rkt" "big-crunch.rkt" "on-frame.rkt")

(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 1
                            #:roughness 0.2))

(big-bang3d/big-crunch
 DEFAULT-STATE
 #:name "Space Orbs"
 #:on-draw on-draw
 #:on-key on-key
 #:on-release on-release
 #:on-mouse on-mouse
 #:stop-state? stop-state?
 #:on-frame on-frame)