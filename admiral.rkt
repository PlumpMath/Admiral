#lang racket

#| This is a real fun game.
To play the game you write a program that plays the game.
Have fun!
|#

(require 2htdp/universe
         2htdp/image
         "engine.rkt"
         "render.rkt"
         "systems/physics.rkt"
         "systems/rules.rkt")

(define SCREEN-WIDTH 800)
(define SCREEN-HEIGHT 600)
(define TICKS-PER-SECOND 60)

;;;; GameWorld
;; Internally there's a hash-map of entities to hash-map of
;; components. That should be an implementation detail though, just
;; about everything is abstract on top of that.
(define a-world (gamestate
                 `()
                 #hash(("player-ship" . #hash((position . (500 100))
                                              (rotation . 90)
                                              (model . ship)
                                              (faction . "blue")
                                              (rockets . (#f #f #f #f #f))))
                       ;; ("enemy-ship" . #hash((position . (400 400))
                       ;;                       (rotation . 0)
                       ;;                       (model . ship)
                       ;;                       (faction . "black")
                       ;;                       (rockets . (#t #t #f #t #f))))
                       ;; ("enemy-ship2" . #hash((position . (420 420))
                       ;;                        (rotation . 0)
                       ;;                        (model . ship)
                       ;;                        (faction . "green")
                       ;;                        (rockets . (#t #t #t #t #f))))
                       ;; ("enemy-ship3" . #hash((position . (430 350))
                       ;;                        (rotation . 0)
                       ;;                        (model . ship)
                       ;;                        (faction . "red")
                       ;;                        (rockets . (#t #f #f #t #f))))
                       ;; ("enemy-ship9" . #hash((position . (100 300))
                       ;;                        (rotation . 0)
                       ;;                        (model . ship)
                       ;;                        (faction . "yellow")
                       ;;                        (rockets . (#t #f #t #f #f))))
                       ;; ("another-ship" . #hash((position . (500 350))
                       ;;                         (rotation . 12)
                       ;;                         (model . ship)
                       ;;                         (faction . "purple")
                       ;;                         (rockets . (#f #t #t #f #t))))
                       )))

;; LOGIC STUFF
;; So far this just returns the rockets, later user will need to be
;; able to do more stuff.

(define update
  (system-updater
   logic-rockets
   (get-apply-rockets-system SCREEN-WIDTH SCREEN-HEIGHT)))

(define (start-scene)
  (big-bang a-world
            (on-tick update (/ 1 TICKS-PER-SECOND))
            (to-draw (get-render-function SCREEN-WIDTH
                                          SCREEN-HEIGHT))))

;;(start-scene)
