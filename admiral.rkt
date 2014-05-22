#lang racket

#| This is a real fun game.
To play the game you write a program that plays the game.
Have fun!
|#

(require 2htdp/universe
         2htdp/image
         "config.rkt"
         "engine.rkt"
         "update.rkt"
         "render.rkt")

;;(define SCREEN-WIDTH 800)
;;(define SCREEN-HEIGHT 600)
;;(define TICKS-PER-SECOND 60)

;;(define update
;;  (system-updater
;;   logic-rockets
;;   (get-apply-rockets-system SCREEN-WIDTH SCREEN-HEIGHT)))

(define (start-scene)
  (big-bang a-world
            (on-tick update (/ 1 TICKS-PER-SECOND))
            (to-draw render)))

(start-scene)
