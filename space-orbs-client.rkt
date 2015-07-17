#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "structures.rkt" "variables.rkt" "on-draw.rkt" "key-events.rkt" "on-mouse.rkt")

(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 1
                            #:roughness 0.2))

(big-bang3d (orbs (orb DEFAULTPOS 0 '() DEFAULTDIR 0 empty 0) (orb DEFAULTPOS2 0 '() DEFAULTDIR 0 empty 0))
            #:name "Space Orbs"
            #:on-draw on-draw
            #:on-key on-key
            #:on-release on-release
            #:on-mouse on-mouse)