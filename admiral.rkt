#lang racket

#| This is a real fun game.
   To play the game you write a program that plays the game.
   Have fun!
|#

(require 2htdp/universe
         2htdp/image
         racket/match)

(define SCREEN-WIDTH 1000)
(define SCREEN-HEIGHT 500)
(define TICKS-PER-SECOND 60)

(struct gamestate (entities) #:transparent)

;;;; GameWorld
;; Internally there's a hash-map of entities to hash-map of components. That
;; should be an implementation detail though, just about everything is abstract
;; on top of that.
(define a-world (gamestate
                 #hash(("player-ship" . #hash((position . (100 100))
                                              (rotation . 90)
                                              (model . ship)
                                              (faction . "blue")
                                              (rockets . (#f #f #f #f #t))))
                       ("enemy-ship" . #hash((position . (400 400))
                                             (rotation . 0)
                                             (model . ship)
                                             (faction . "black")
                                             (rockets . (#t #t #f #t #f))))
                       ("enemy-ship2" . #hash((position . (420 420))
                                             (rotation . 0)
                                             (model . ship)
                                             (faction . "green")
                                             (rockets . (#t #t #t #t #f))))
                       ("enemy-ship3" . #hash((position . (430 350))
                                             (rotation . 0)
                                             (model . ship)
                                             (faction . "red")
                                             (rockets . (#t #f #f #t #f))))
                       ("enemy-ship9" . #hash((position . (100 300))
                                             (rotation . 0)
                                             (model . ship)
                                             (faction . "yellow")
                                             (rockets . (#t #f #t #f #f))))
                       ("another-ship" . #hash((position . (500 450))
                                               (rotation . 12)
                                               (model . ship)
                                               (faction . "purple")
                                               (rockets . (#t #f #t #t #t)))))))

(define (has-component? entity component-type)
  (hash-has-key? entity component-type))

(define (get-component entity component-type)
  (hash-ref entity component-type #f))

;;;; Graphics
;; This is a bit messy and sort of locked to 1000 x 500
(define (background-grid)
  (define with-x
    (for/fold ([scene (empty-scene SCREEN-WIDTH SCREEN-HEIGHT "darkgray")])
              ([x (stream-map (λ (x) (* 10 x)) (in-range 1 100))])
      (scene+line
       scene
       x 0 x SCREEN-HEIGHT "lightgray"
       )))
  (for/fold ([scene with-x])
            ([y (stream-map (λ (x) (* 10 x)) (in-range 1 50))])
    (scene+line
     scene
     0 y SCREEN-WIDTH y "lightgray"
     ))
  )

(define (ship faction)
  (let ([body (rectangle 20 40 "solid" faction)]
        [engine (rectangle 15 30 "solid" faction)])
    (overlay/offset (overlay/offset engine 20 0 engine) 0 -15 body)))

(define (add-rockets ship rockets)
  (define rocket (triangle 15 "solid" "gold"))
  (define blank (triangle 15 0 "black"))
  
  (define (add-port-rockets bp bs ship)
    (define p (rotate 90 (if bp rocket blank)))
    (define s (rotate -90 (if bs rocket blank)))
    (define port-rockets (overlay/offset p 32 0 s))
    (overlay/offset port-rockets 0 18 ship))
  
  (define (add-stern-rockets sp ss ship)
    (define p (rotate 90 (if sp rocket blank)))
    (define s (rotate -90 (if ss rocket blank)))
    (define stern-rockets (overlay/offset p 48 0 s))
    (overlay/offset stern-rockets 0 -17 ship))
  
  (define (add-boosters boost ship)
    (define booster (rotate 180 rocket))
    (define boosters (overlay/offset booster 20 0 booster))
    (if boost
        (overlay/offset boosters 0 -31 ship)
        ship))
  
  (match rockets
    [(list bp bs sp ss boost)
     (add-boosters boost
                   (add-stern-rockets sp ss
                                      (add-port-rockets bp bs ship)))]))

(define (get-sprite model faction)
  (cond
    [(equal? model 'ship) (ship faction)]))

(define (draw-entity scene ent)
  (define model (get-component ent 'model))
  (define position (get-component ent 'position))
  (define faction (get-component ent 'faction))
  (define rotation (get-component ent 'rotation))
  (define rockets (get-component ent 'rockets))
  (define sprite (get-sprite model faction))
  (define with-rockets (add-rockets sprite rockets))
  (place-image (rotate rotation with-rockets)
               (first position) (second position)
               scene))

(define (render-game state)
  ;; Draw entities.
  (define entities (gamestate-entities state))
  (for/fold ([frame (background-grid)])
            ([ent (hash-values entities)])
    (draw-entity frame ent)))

;;;; Systems
;; rocket locations
;;(list bp bs sp ss boost)
;; bp  | |  bs
;; sp | | | ss
;;    boost
(define (realative-ship-force rockets)
  ;; forces here represent (rotational, x, y)
  (define forces '((-2 2 0) (2 -2 0) (2 2 0) (-2 -2 0) (0 0 5)))

  (define relative-force
    (for/fold ([cumulative-force '(0 0 0)])
              ([rocket-on? rockets]
               [rocket-force forces]
               #:when rocket-on?)
      (map + cumulative-force rocket-force)))
  
  relative-force)

(define (is-ship? kvp)
  (define ent (cdr kvp))
  
  (and 
   (has-component? ent 'rockets)
   (has-component? ent 'position)
   (has-component? ent 'rotation)))

(define (apply-rockets state)
  (define ents (gamestate-entities state))
  
  (define new-ents
    (for/fold ([ents ents])
              ([kvp (hash->list ents)]
               #:when (is-ship? kvp))
      (define id (car kvp))
      (define ent (cdr kvp))
      (define rockets (get-component ent 'rockets))
      (define position (get-component ent 'position))
      (define rotation (get-component ent 'rotation))
      (define relative-forces (realative-ship-force rockets))
      ;; We need the forces in terms of the rotation of the ship.
      (define current-rotational-vector
        (let ([x (second relative-forces)]
              [y (third relative-forces)]
              [theta (degrees->radians rotation)])
          (list (- (* x (cos theta)) (* y (sin theta)))
                (+ (* x (sin theta)) (* y (cos theta))))))
      (define translation (map * current-rotational-vector (rest relative-forces)))
      (define new-rotation (modulo (+ rotation (first relative-forces)) 360)) 
      (define new-position (map + translation position))
      
      (define new-comps
        (hash-set* ent 'rotation new-rotation 'position new-position))
      
      (hash-set ents id new-comps)))
  
    (struct-copy gamestate state [entities new-ents]))

(define (update-game state)
  (define update-function
    (compose apply-rockets))
  (update-function state))

(define (start-scene)
  (big-bang a-world
            (on-tick update-game (/ 1 TICKS-PER-SECOND))
            ;;(on-key keydown)
            ;;(on-release keyup)
            (to-draw render-game)))

(start-scene)
