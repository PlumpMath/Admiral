#lang racket
(provide a-world update)

(require "engine.rkt"
         "systems/physics.rkt"
         "systems/rules.rkt")

;; Sample Rules
(define sample-rules
  (lambda (pos-x pos-y rotation)
    `((IFF ,(< pos-x 100) near-left-border)
      (IFF ,(> pos-x 700) near-right-border)
      (IFF ,(= rotation 90) facing-left)
      (IFF ,(= rotation 270) facing-right)
      (IFF rotate-clockwise bow-starboard-rocket)
      (IFF rotate-clockwise stern-port-rocket)
      
      (IF (AND near-left-border (NOT facing-right))
          rotate-clockwise)
      (IF (AND near-right-border (NOT facing-left))
          rotate-clockwise)
    
      (IF (AND (NOT near-right-border)
               (NOT near-left-border))
          booster-rocket)
    
      (IF (AND near-left-border facing-right)
          booster-rocket)
    
      (IF (AND near-right-border facing-left)
          booster-rocket)
    
      (IFF (NOT rotate-clockwise) booster-rocket))))

;; Example valid world.
(define a-world (gamestate
                 sample-rules
                 #hash(("player-ship" . #hash((position . (500 100))
                                              (rotation . 90)
                                              (model . ship)
                                              (faction . "teal")
                                              (rockets . (#f #f #f #f #f)))))))

(define update
  (system-updater
   rocket-logic-system
   rocket-physics-system))