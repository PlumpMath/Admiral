#lang racket
(provide rocket-physics-system)

(require "../config.rkt"
         "../engine.rkt")

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

(define rocket-physics-system
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
                [(> x (WORLD-WIDTH)) (WORLD-WIDTH)]
                [else x])
               (cond
                [(< y 0) 0]
                [(> y (WORLD-HEIGHT)) (WORLD-HEIGHT)]
                [else y]))))
     ;; alias this with modify-components or something so it's more
     ;; game like.
     (hash-set* components
                'rotation new-rotation
                'position ghetto-wall-collision-position))))

(module+ test (require rackunit)
  (check-equal? 1 1)
  (check-equal? (gamespace-force 90 '(1 1)) '(1.0 -1.0))
  (check-equal? (gamespace-force 180 '(1 1)) '(-1.0 -1.0))
  ;(check-equal? (gamespace-force 45 '(1 1)) '(1 -1))
  )