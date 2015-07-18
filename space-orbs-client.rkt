#lang racket
(require pict3d pict3d/universe mzlib/string "structures.rkt" "variables.rkt" "on-draw.rkt" "key-events.rkt" "on-mouse.rkt" "stop-state.rkt" "big-crunch.rkt")

(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 1
                            #:roughness 0.2))

(big-bang3d/big-crunch
 (game
  (orbs (orb DEFAULTPOS 0 '() DEFAULTDIR 0 empty 0) (orb DEFAULTPOS2 0 '() DEFAULTDIR 0 empty 0))
  #f)
 #:name "Space Orbs"
 #:on-draw on-draw
 #:on-key on-key
 #:on-release on-release
 #:on-mouse on-mouse
 #:stop-state? stop-state?)