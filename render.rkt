#lang racket
#| Code for rendering the gamestate |#

(require 2htdp/image
         "engine.rkt")
(provide render-game)

(define SCREEN-WIDTH 1000)
(define SCREEN-HEIGHT 500)

(define background-grid
  (let ([with-x 
         (for/fold ([scene (empty-scene SCREEN-WIDTH SCREEN-HEIGHT "darkgray")])
             ([x (stream-map (lambda (x) (* 10 x)) (in-range 1 100))])
           (scene+line
            scene
            x 0 x SCREEN-HEIGHT "lightgray"
            ))])
    (for/fold ([scene with-x])
        ([y (stream-map (lambda (x) (* 10 x)) (in-range 1 50))])
      (scene+line
       scene
       0 y SCREEN-WIDTH y "lightgray"
       ))))

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
  (define entities (get-entities state))
  (for/fold ([frame background-grid])
            ([ent (hash-values entities)])
    (draw-entity frame ent)))