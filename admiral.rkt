#lang racket

#| This is a real fun game.
To play the game you write a program that plays the game.
Have fun!
|#

(require 2htdp/universe
         2htdp/image
         "engine.rkt"
         "render.rkt"
         "rules.rkt")

(define SCREEN-WIDTH 1000)
(define SCREEN-HEIGHT 500)
(define TICKS-PER-SECOND 60)

;;;; GameWorld
;; Internally there's a hash-map of entities to hash-map of
;; components. That should be an implementation detail though, just
;; about everything is abstract on top of that.
(define a-world (gamestate
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
(define (get-rockets current-rockets state components)
  (run-logic current-rockets state components))

(define logic-rockets
  (system '(rockets)
    (lambda (state id components)
      (define rockets (get-component components 'rockets))
      (define next-rockets (get-rockets rockets state components))
      (hash-set components 'rockets next-rockets)
      )))

;; --------

;; rocket locations
;;(list bp bs sp ss boost)
;; bp  | |  bs
;; sp | | | ss
;;    boost
(define (realative-ship-force rockets)
  ;; forces here represent (rotational, x, y)
  (define forces '((-2 2 0) (2 -2 0) (2 2 0) (-2 -2 0) (0 0 -5)))

  (for/fold ([cumulative-force '(0 0 0)])
      ([rocket-on? rockets]
       [rocket-force forces]
       #:when rocket-on?)
    (map + cumulative-force rocket-force)))

;; Takes the relative force on the ship and the ships rotation and
;; returns that force in gamespace. Note that this x and y still use
;; the same coordinate system as gamespace (positive y being down).
;; But relative to the ship so if force-x = 0 and force-y = -5 that
;; means the ship is pushing forward at a force of 5.
;; Ships rotation is counter-clockwise and in degrees as that's what
;; racket's image library works with.
(define (gamespace-force ship-rotation relative-force)
  (let ([theta (degrees->radians (modulo (- 360 ship-rotation) 360))]
        [rx (first relative-force)]
        [ry (second relative-force)])
    (map round-to-2-places
         (list (- (* rx (cos theta)) (* ry (sin theta)))
               (+ (* rx (sin theta)) (* ry (cos theta)))))))

;; Not really sure what resolution I need.
(define (round-to-2-places n)
  (/ (round (* 100 n)) 100))

(define apply-rockets
  (system '(rockets position rotation)
   (lambda (state id components)
     (define rockets (get-component components 'rockets))
     (define position (get-component components 'position))
     (define rotation (get-component components 'rotation))
     (define relative-forces (realative-ship-force rockets))
     (define global-force (gamespace-force rotation (rest relative-forces)))
     (define new-rotation (modulo (+ rotation (first relative-forces)) 360))
     (define new-position (map + global-force position))
     (define ghetto-wall-collision-position
       (let ([x (first new-position)]
             [y (second new-position)])
         (list (cond
                [(< x 0) 0]
                [(> x SCREEN-WIDTH) SCREEN-WIDTH]
                [else x])
               (cond
                [(< y 0) 0]
                [(> y SCREEN-HEIGHT) SCREEN-HEIGHT]
                [else y]))))
     ;; alias this with modify-components or something so it's more
     ;; game like.
     (hash-set* components
                'rotation new-rotation
                'position ghetto-wall-collision-position))))

(define update
  (system-updater
   logic-rockets
   apply-rockets))

(define (start-scene)
  (big-bang a-world
            (on-tick update (/ 1 TICKS-PER-SECOND))
            (to-draw (get-render-function SCREEN-WIDTH
                                          SCREEN-HEIGHT))))

;;(start-scene)

;; Tests, will need breaking out at the same time as the file is
;; broken out.
(module+ test (require rackunit)
  (check-equal? 1 1)
  (check-equal? (gamespace-force 90 '(1 1)) '(1.0 -1.0))
  (check-equal? (gamespace-force 180 '(1 1)) '(-1.0 -1.0))
  ;(check-equal? (gamespace-force 45 '(1 1)) '(1 -1))
  )