#lang racket
(provide gamestate has-component? get-component
         has-comps? system system-updater
         get-entities get-rules set-rules)

(struct gamestate (rules entities) #:transparent)

(define (get-rules state)
  (gamestate-rules state))

(define (set-rules state new-rules)
  (struct-copy gamestate state [rules new-rules]))

(define (get-entities state)
  (gamestate-entities state))

(define (has-component? entity component-type)
  (hash-has-key? entity component-type))

(define (get-component entity component-type)
  (hash-ref entity component-type #f))

(define (has-comps? kvp required-components)
  (define ent (cdr kvp))
  (for/fold
      ([has-all? #t])
      ([has-comp? (map (curry hash-has-key? ent) required-components)])
    (and has-all? has-comp?)))

;; Body must return new component hash-map. Will add other variations
;; later if I need to that allow modification of other entities and
;; adding and removing entities.
;; (body state id components) => components
;; TODO: Other system functions
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

(define (system-updater . systems)
  (define update-fn (apply compose (reverse systems)))
  (lambda (state)
    (update-fn state)))

(module+ test-engine (require rackunit)
  (define an-entity '("entity-id" . #hash((position . (100 100))
                                          (magic-swag . "holla"))))
  (check-true (has-comps? an-entity '(position magic-swag)))
  (check-false (has-comps? an-entity '(position evil-powers)))
  )