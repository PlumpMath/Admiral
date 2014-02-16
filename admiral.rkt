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
;; Internally there's a hash-map of entities to hash-map of
;; components. That should be an implementation detail though, just
;; about everything is abstract on top of that.
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
                       ("another-ship" . #hash((position . (500 350))
                                               (rotation . 12)
                                               (model . ship)
                                               (faction . "purple")
                                               (rockets . (#f #t #t #f #t))))
                       )))

(define (has-component? entity component-type)
  (hash-has-key? entity component-type))

(define (get-component entity component-type)
  (hash-ref entity component-type #f))

;;;; Graphics
;; This is a bit messy and sort of locked to 1000 x 500
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
  (define entities (gamestate-entities state))
  (for/fold ([frame background-grid])
            ([ent (hash-values entities)])
    (draw-entity frame ent)))

;;;; Systems

;; Lets try and get rid of some of this boilerplate.

;; Systems are called with each entity that has all the components.
;; They should return a new entity. I need to also give them access
;; to the whole gamestate somehow so they can query it. Not sure yet
;; if this should just return a lambda that gets the defines for us or if
;; it should instead return something that hooks into a more
;; efficiciant way to precompute what's needed.

;; Making this a macro will make the syntax a bit nicer.

;; Typed racket is another option to make sure the body is correct.
;; first, body has to be a function that can be called
;; (body state id components). It is called once with each entity with all
;; required-components.
;; but what does it return....
;; Maybe it should return a map of ids to entities. That way it can
;; update any entities it wants, but that wont let it remove
;; entities...
;; This is one of the problems with functional purity, if you could
;; just set! on any entities (like how all other game engines work)
;; this wouldn't be a problem.
;; Another thing to do is to return a full gamestate and just add a
;; few helpers for (update-entity or whatever). Yet another thing to
;; do is have a few variations on system that can return different
;; things.
;; I actuially really like that idea, just make em when you need em.

;; This seems overly verbose... Need to learn more racket probaby.
(define (has-comps? kvp required-components)
  (define ent (cdr kvp))
  (for/fold
      ([has-all? #t])
      ([has-comp? (map (curry hash-has-key? ent) required-components)])
    (and has-all? has-comp?)))

(module+ test-has-comps (require rackunit)
  (define an-entity '("entity-id" . #hash((position . (100 100))
                                          (magic-swag . "holla"))))
  (check-true (has-comps? an-entity '(position magic-swag)))
  (check-false (has-comps? an-entity '(position evil-powers)))
  )

;; Body must return new component hash-map. Will add other variations
;; later if I need to that allow modification of other entities and
;; adding and removing entities.
;; (body state id components) => components
(define (system required-components body)
  (lambda (state)
    (define entities (gamestate-entities state))
    (define new-ents
      (for/fold
          ([ents entities])
          ([kvp (hash->list entities)]
           #:when (has-comps? kvp required-components))
        (define id (car kvp))
        (define comps (cdr kvp))
        (define new-comps (body state id comps))
        (hash-set ents id new-comps)))
    (struct-copy gamestate state [entities new-ents])))

(module+ test-system (require rackunit)
  (define an-entity '("entity-id" . #hash((position . (100 100))
                                          (magic-swag . "holla"))))
  (check-true (has-comps? an-entity '(position magic-swag)))
  (check-false (has-comps? an-entity '(position evil-powers)))
  )

;; First test of ship controlling, lets just fuck around with the
;; rockets every frame and see what happens.
(define (rand-bool)
  (< (random) 0.75))

(define random-rockets
  (system '(rockets)
   (lambda (state id components)
     (define rockets (get-component components 'rockets))
     ;; (define new-rockets
     ;;   (for/list ([x (range 5)])
     ;;     (rand-bool)))
     (define new-rockets `(#f #f #t #f #t))
     (hash-set components 'rockets new-rockets)
     )))

;; LOGIC STUFF

(define (get-rockets current-rockets state components)
  ;; State is the gamestate, I think I need to derive a bunch of shit
  ;; from them.

  ;; lets pick a real simple rule to start with.
  ;; If location is something turn to face something, drive.
  ;; Want the ship to fly back and forth.

  ;; Rules
  ;; if location-x is < 10
  ;;   if rotation = 270
  ;;     boost
  ;;   else
  ;;     rotate clockwise
  ;; if location-y is > 990
  ;;   if rotation - 90
  ;;     boost
  ;;   else
  ;;     rotate clockwise

  ;; First the real shitty version!
  (define position (get-component components 'position))
  (define rotation (get-component components 'rotation))
  (define pos-x (first position))
  (define boost `(#f #f #f #f #t))
  (define rotate `(#f #t #t #f #f))
  (cond
   [(< pos-x 40) (if (= rotation 270)
                 boost
                 rotate)]
   [(> pos-x 960) (if (= rotation 90)
                  boost
                  rotate)]
   [else boost])
  
  ;; (first
  ;;  (run 1 (q)
  ;;       (fresh (bp bs sp ss boost)
  ;;              (== `(,bp ,bs ,sp ,ss ,boost) current-rockets))))

  ;; current-rockets
  )

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

(define (update-game state)
  (define update-function
    (compose
     random-rockets
     apply-rockets))
  (update-function state))

(define (start-scene)
  (big-bang a-world
            (on-tick update-game (/ 1 TICKS-PER-SECOND))
            ;;(on-key keydown)
            ;;(on-release keyup)
            (to-draw render-game)))

;;(start-scene)

;; Tests, will need breaking out at the same time as the file is
;; broken out.
(module+ test (require rackunit)
  (check-equal? 1 1)
  (check-equal? (gamespace-force 90 '(1 1)) '(1.0 -1.0))
  (check-equal? (gamespace-force 180 '(1 1)) '(-1.0 -1.0))
  ;(check-equal? (gamespace-force 45 '(1 1)) '(1 -1))
  )
